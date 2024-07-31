open Printf
open Sqlite3

let assert_ok rc = assert (rc = Rc.OK)
let assert_done rc = assert (rc = Rc.DONE)

let column_decltype s i =
  match column_decltype s i with None -> "<NONE>" | Some str -> str

let stepbystep s =
  assert_done
    (iter s ~f:(function r ->
         Array.iteri
           (fun i c ->
             printf "%s column[%d] %s = %s\n%!" (column_decltype s i) i
               (column_name s i) (Data.to_string_coerce c))
           r))

let stepbystep_wrong s =
  while step s = Rc.ROW do
    for i = 0 to data_count s do
      printf "%s column[%d] %s = %s\n%!" (column_decltype s i) i
        (column_name s i)
        (Data.to_string_coerce (column s i))
    done
  done

let mk_tbl db id =
  let sql =
    sprintf "CREATE TABLE tbl%d (a varchar(1), b INTEGER, c FLOAT)" id
  in
  Rc.check (exec db sql);
  let sql = sprintf "INSERT INTO tbl%d VALUES ('a', 3, 3.14)" id in
  Rc.check (exec db sql)

let%test "test_stmt" =
  let db = db_open "t_stmt" in

  mk_tbl db 0;
  mk_tbl db 1;

  assert_ok (exec db "DROP TABLE IF EXISTS test0");
  assert_ok (exec db "DROP TABLE IF EXISTS test1");
  assert_ok (exec db "CREATE TABLE test0(a TEXT, b INTEGER, c REAL)");
  assert_ok (exec db "CREATE TABLE test1(a TEXT, b INTEGER, c REAL)");
  assert_ok (exec db "INSERT INTO test0 VALUES ('a', 1, 0.1), ('b', 2, 0.2)");
  assert_ok (exec db "INSERT INTO test1 VALUES ('c', 3, 0.3), ('d', 4, 0.4)");

  (* Test the finalization... *)
  for _i = 0 to 100 do
    (* printf "Create statement %d\n%!" i; *)
    let sql = sprintf "SELECT * FROM test0" in
    ignore (prepare db sql)
  done;

  for _i = 0 to 100 do
    (* printf "Create statement %d\n%!" i; *)
    let sql = sprintf "SELECT * FROM test0" in
    assert_ok (finalize (prepare db sql))
  done;

  for _i = 0 to 100 do
    (* printf "Create statement %d\n%!" i; *)
    let sql = sprintf "SELECT * FROM test0; SELECT * FROM test1;" in
    ignore (prepare_tail (prepare db sql))
  done;

  let premade_statement = ref None in
  for _i = 0 to 100 do
    (* printf "Create statement %d\n%!" i; *)
    let sql = sprintf "SELECT * FROM test0; SELECT * FROM test1;" in
    ignore (prepare_or_reset db premade_statement sql)
  done;

  for _i = 1 to 10 do
    (* printf "Create statement %d\n%!" i; *)
    let sql = sprintf "SELECT * FROM test0; SELECT * FROM test1;" in
    let stmt = prepare db sql in
    assert_ok (finalize stmt);
    try ignore (prepare_tail stmt) with _xcp -> ()
  done;

  let sql = sprintf "SELECT * FROM test0; SELECT * FROM test0;" in
  let stmt = prepare db sql in
  print_endline "A-------------------------------------------";
  stepbystep stmt;
  print_endline "B-------------------------------------------";
  (match prepare_tail stmt with
  | Some s -> stepbystep s
  | None -> failwith "Tail not found!");
  assert_ok (reset stmt);
  print_endline "C-------------------------------------------";
  stepbystep stmt;
  print_endline "D-------------------------------------------";
  (match prepare_tail stmt with
  | Some s -> stepbystep s
  | None -> failwith "Tail not found!");
  (match prepare_tail stmt with
  | Some s -> stepbystep s
  | None -> failwith "Tail not found!");
  print_endline "E-------------------------------------------";
  (try
     match prepare_tail stmt with
     | Some s -> stepbystep_wrong s
     | None -> failwith "Tail not found!"
   with xcp -> printf "Ok: %s\n" (Printexc.to_string xcp));
  assert_ok (finalize stmt);

  let stmt = prepare db "SELECT * FROM test0 WHERE a = ? AND b = ?" in
  assert_ok (bind_values stmt [ Data.TEXT "a"; Data.INT 1L ]);
  assert (step stmt = Rc.ROW);
  assert_ok (finalize stmt);
  let stmt = prepare db "SELECT * FROM test0 WHERE a = :a" in
  assert_ok (bind_name stmt ":a" (Data.TEXT "b"));
  assert (step stmt = Rc.ROW);
  assert_ok (finalize stmt);
  let stmt = prepare db "SELECT * FROM test0 WHERE a = :a AND b = :b" in
  assert_ok (bind_names stmt [ (":a", Data.TEXT "a"); (":b", Data.INT 1L) ]);
  assert (step stmt = Rc.ROW);
  assert_ok (finalize stmt);
  let stmt = prepare db "SELECT * FROM test0 WHERE a = ?" in
  (try assert_ok (bind_values stmt [ Data.INT 1L; Data.INT 2L ]) with
  | RangeError _ -> ()
  | exn -> raise exn);
  (try assert_ok (bind_name stmt ":a" (Data.INT 3L)) with
  | Not_found -> ()
  | exn -> raise exn);
  assert_ok (finalize stmt);

  let of_intdata = function Data.INT i -> i | _ -> failwith "Invalid type" in
  let stmt = prepare db "SELECT b FROM test0" in
  let rc, sum =
    fold stmt ~init:0L ~f:(fun s b -> Int64.add s (of_intdata b.(0)))
  in
  assert_ok (finalize stmt);
  assert_done rc;
  assert (sum = 3L);
  printf "fold: sum of table0(b) is %Ld\n" sum;

  Gc.full_major ();

  (* Collect any dangling statements *)
  assert (db_close db);
  true
