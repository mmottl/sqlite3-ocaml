open Sqlite3

let%test "test_db" =
  for _ = 1 to 1000 do
    let db = db_open "t" in
    Gc.full_major ();
    let _ = db_close db in
    ()
  done;
  print_endline "Able to open and close database";
  true
  ;;