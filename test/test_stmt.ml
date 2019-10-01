open Sqlite3

let column_decltype s i =
  match column_decltype s i with
  | None -> "<NONE>"
  | Some str -> str

let stepbystep s =
  while step s = Rc.ROW do
    for i = 0 to data_count s - 1 do
      Printf.printf "%s column[%d] %s = %s\n%!"
        (column_decltype s i) i
        (column_name s i)
        (Data.to_string_coerce (column s i))
    done
  done

let stepbystep_wrong s =
  while step s = Rc.ROW do
    for i = 0 to data_count s do
      Printf.printf "%s column[%d] %s = %s\n%!"
        (column_decltype s i) i
        (column_name s i)
        (Data.to_string_coerce (column s i))
    done
  done

let%test "test_stmt" =
  (* Force test_exec test first *)
  let () = Test_exec.dep in

  let db = db_open "t" in

  (* Test the finalization... *)
  for _i = 0 to 100 do
    (* Printf.printf "Create statement %d\n%!" i; *)
    let sql = Printf.sprintf "SELECT * FROM tbl0" in
    ignore (prepare db sql)
  done;

  for _i = 0 to 100 do
    (* Printf.printf "Create statement %d\n%!" i; *)
    let sql = Printf.sprintf "SELECT * FROM tbl0" in
    ignore (finalize (prepare db sql))
  done;

  for _i = 0 to 100 do
    (* Printf.printf "Create statement %d\n%!" i; *)
    let sql = Printf.sprintf "SELECT * FROM tbl0; SELECT * FROM tbl1;" in
    ignore (prepare_tail (prepare db sql))
  done;

  let premade_statement = ref None in
  for _i = 0 to 100 do
    (* Printf.printf "Create statement %d\n%!" i; *)
    let sql = Printf.sprintf "SELECT * FROM tbl0; SELECT * FROM tbl1;" in
    ignore (prepare_or_reset db premade_statement sql)
  done;

  for _i = 1 to 10 do
    (* Printf.printf "Create statement %d\n%!" i; *)
    let sql = Printf.sprintf "SELECT * FROM tbl0; SELECT * FROM tbl1;" in
    let stmt = prepare db sql in
    ignore (finalize stmt);
    try ignore (prepare_tail stmt)
    with _xcp -> ()
  done;

  let sql = Printf.sprintf "SELECT * FROM tbl0; SELECT * FROM tbl0;" in
  let stmt = prepare db sql in
  print_endline "A-------------------------------------------";
  stepbystep stmt;
  print_endline "B-------------------------------------------";
  (match prepare_tail stmt with
  | Some s -> stepbystep s
  | None -> failwith "Tail not found!");
  ignore (reset stmt);
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
  try
    match prepare_tail stmt with
    | Some s -> stepbystep_wrong s; false
    | None -> failwith "Tail not found!"
  with xcp -> Printf.printf "Ok: %s\n" (Printexc.to_string xcp); true
