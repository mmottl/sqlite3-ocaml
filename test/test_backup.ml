open Sqlite3

let%test "test_backup" =

  (* Sql statements for this test *)
  let schema = "CREATE TABLE test_backup (num INTEGER NOT NULL, string TEXT NULL);" 
  in
  let insert_sql = "INSERT INTO test_backup (num, string) VALUES (?, ?)"
  in
  let select_sql = "SELECT num, string FROM test_backup"
  in

  (* Construct database and statements *)
  let src = db_open "t_backup_src" in
  let rc = exec src schema in
  Printf.printf "Created schema: %s\n" (Rc.to_string rc);
  let insert_stmt = prepare src insert_sql in

  (* Insert values in row 1 *)
  for x = 0 to 1000 do
    ignore (reset insert_stmt);
    ignore (bind insert_stmt 1 (Sqlite3.Data.INT (Int64.of_int x)));
    ignore (bind insert_stmt 2 (Data.opt_text (Some (string_of_int x))));
    ignore (step insert_stmt);
  done;
  Printf.printf "Data written to database\n";

  (* Clean up *)
  ignore (finalize insert_stmt);

  (* Create a backup of the database *)
  let dst = db_open "t_backup_dst" in
  let backup = Backup.init ~dst ~dst_name:"main" ~src ~src_name:"main" in
  let rec run () =
    match Backup.step backup 1 with
    | Rc.LOCKED | Rc.BUSY | Rc.OK -> run ()
    | Rc.DONE -> Printf.printf "Backup complete\n"
    | _ -> assert (true)
  in
    run ();
  ignore (Backup.finish backup);

  (* Fetch data back with values *)
  let select_stmt = prepare dst select_sql in
  ignore (reset select_stmt);
  let rec run () =
    match step select_stmt with
    | Rc.ROW -> assert ((string_of_int (Data.to_int_exn (column select_stmt 0))) = Data.to_string_exn (column select_stmt 1));
                run ();
    | Rc.DONE -> ()
    | _ -> assert (true)
  in
    run ();
  Printf.printf "Data read from backup database\n";

  (* Clean up *)
  ignore (finalize select_stmt);
  assert (db_close src);
  assert (db_close dst);
  true

