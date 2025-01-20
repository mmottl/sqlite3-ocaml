open Sqlite3

let assert_ok rc = assert (rc = Rc.OK)
let assert_error rc = assert (rc = Rc.ERROR)

let assert_rows_equal expected_rows db sql =
  let actual_rows = ref [] in
  let _ =
    assert_ok
      (exec db sql ~cb:(fun row _ ->
           match row.(0) with
           | Some a -> actual_rows := a :: !actual_rows
           | _ -> ()))
  in
  let actual_rows = List.sort compare (List.rev !actual_rows) in
  assert (actual_rows = expected_rows)

let%test "test_collation" =
  let db = db_open "t_collation" in
  create_collation db "FIRST_CHAR" (fun left right ->
      compare (String.get left 0) (String.get right 0));

  let found_first_char = ref false in
  let _ =
    assert_ok
      (exec db "PRAGMA collation_list" ~cb:(fun row _ ->
           match row.(1) with
           | Some a -> found_first_char := !found_first_char || a = "FIRST_CHAR"
           | _ -> ()))
  in
  assert !found_first_char;

  assert_ok (exec db "DROP TABLE IF EXISTS tbl");
  assert_ok (exec db "CREATE TABLE tbl (a varchar(10) COLLATE FIRST_CHAR)");
  assert_ok (exec db "INSERT INTO tbl VALUES ('pippo')");
  assert_ok (exec db "INSERT INTO tbl VALUES ('pippo2')");
  assert_ok (exec db "INSERT INTO tbl VALUES ('atypical')");
  assert_rows_equal [ "pippo"; "pippo2" ] db
    "SELECT * FROM tbl WHERE a = 'pippo'";
  assert_rows_equal [ "pippo"; "pippo2" ] db
    "SELECT * FROM tbl WHERE a = 'papa'";
  assert_rows_equal [ "atypical" ] db
    "SELECT * FROM tbl WHERE a = 'asymmetrical'";
  assert_rows_equal [ "atypical" ] db "SELECT * FROM tbl WHERE a = 'atypical'";
  assert_rows_equal [] db "SELECT * FROM tbl WHERE a = 'border'";

  assert_ok (exec db "DROP TABLE IF EXISTS tbl");
  assert_ok (exec db "CREATE TABLE tbl (a varchar(10))");
  assert_ok (exec db "INSERT INTO tbl VALUES ('pippo')");
  assert_ok (exec db "INSERT INTO tbl VALUES ('pippo2')");
  assert_rows_equal [ "pippo" ] db "SELECT * FROM tbl WHERE a = 'pippo'";
  assert_rows_equal [ "pippo"; "pippo2" ] db
    "SELECT * FROM tbl WHERE a = 'pippo' COLLATE FIRST_CHAR";

  delete_collation db "FIRST_CHAR";
  assert_error
    (exec db "SELECT * FROM tbl WHERE a = 'pippo' COLLATE FIRST_CHAR");
  true
