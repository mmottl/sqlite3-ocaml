open Sqlite3

let%test "test_values" =

  (* Sql statements for this test *)
  let schema = "CREATE TABLE test_values ( " ^
  "    row_id INTEGER NOT NULL, " ^
  "    string_col TEXT NULL, " ^
  "    int_col INT NULL, " ^
  "    int64_col INT NULL, " ^
  "    float_col FLOAT NULL, " ^
  "    bool_col INT NULL" ^
  ");" 
  in
  let insert_sql = "INSERT INTO test_values " ^
    "(row_id, string_col, int_col, int64_col, float_col, bool_col) " ^
    "VALUES (?, ?, ?, ?, ?, ?)"
  in
  let select_sql = "SELECT " ^
    "string_col, int_col, int64_col, float_col, bool_col " ^
    "FROM test_values WHERE row_id = ?"
  in

  (* Construct database and statements *)
  let db = db_open "t_values" in
  let rc = exec db schema in
  Printf.printf "Created schema: %s" (Rc.to_string rc);
  let insert_stmt = prepare db insert_sql in
  let select_stmt = prepare db select_sql in

  (* Insert values in row 1 *)
  let test_float_val = 56.789 in
  ignore (reset insert_stmt);
  ignore (bind insert_stmt 1 (Sqlite3.Data.INT 1L));
  ignore (bind insert_stmt 2 (Data.opt_text (Some "Hi Mom")));
  ignore (bind insert_stmt 3 (Data.opt_int (Some 1)));
  ignore (bind insert_stmt 4 (Data.opt_int64 (Some Int64.max_int)));
  ignore (bind insert_stmt 5 (Data.opt_float (Some test_float_val)));
  ignore (bind insert_stmt 6 (Data.opt_bool (Some true)));
  ignore (step insert_stmt);

  (* Insert nulls in row 2 *)
  ignore (reset insert_stmt);
  ignore (bind insert_stmt 1 (Sqlite3.Data.INT 2L));
  ignore (bind insert_stmt 2 (Data.opt_text None));
  ignore (bind insert_stmt 3 (Data.opt_int None));
  ignore (bind insert_stmt 4 (Data.opt_int64 None));
  ignore (bind insert_stmt 5 (Data.opt_float None));
  ignore (bind insert_stmt 6 (Data.opt_bool None));
  ignore (step insert_stmt);

  (* Fetch data back with values *)
  ignore (reset select_stmt);
  ignore (bind select_stmt 1 (Sqlite3.Data.INT 1L));
  if Sqlite3.step select_stmt = Sqlite3.Rc.ROW then begin
    assert (Data.to_string_exn (column select_stmt 0) = "Hi Mom");
    assert (Data.to_int_exn (column select_stmt 1) = 1);
    assert (Data.to_int64_exn (column select_stmt 2) = Int64.max_int);
    assert (Data.to_float_exn (column select_stmt 3) = test_float_val);
    assert (Data.to_bool_exn (column select_stmt 4) = true);
  end;

  (* Fetch data back with nulls *)
  ignore (reset select_stmt);
  ignore (bind select_stmt 1 (Sqlite3.Data.INT 2L));
  if Sqlite3.step select_stmt = Sqlite3.Rc.ROW then begin
    assert (column_text select_stmt 0 = "");
    assert (column_int select_stmt 1 = 0);
    assert (column_int32 select_stmt 1 = 0l);
    assert (column_int64 select_stmt 2 = 0L);
    assert (column_double select_stmt 3 = 0.0);
    assert (column_bool select_stmt 4 = false);
  end;

  (* Clean up *)
  ignore (finalize insert_stmt);
  ignore (finalize select_stmt);
  assert (db_close db);
  true

