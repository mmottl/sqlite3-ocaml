open Printf
open Sqlite3

exception Dummy

let assert_ok rc = assert (rc = Rc.OK)

let%test "test_exec" =
  let db = db_open "t_exec" in
  for i = 0 to 10 do
    try
      let drop = sprintf "DROP TABLE IF EXISTS tbl%d" i
      and sql =
        sprintf "CREATE TABLE tbl%d (a varchar(1), b INTEGER, c FLOAT)" i
      in
      printf "%d %s\n%!" i sql;
      assert_ok (exec db drop);
      match exec db sql ~cb:(fun _ _ -> print_endline "???") with
      | Rc.OK -> ()
      | _ ->
          printf "Failed: %s\n" (errmsg db);
          assert false
    with xcp -> print_endline (Printexc.to_string xcp)
  done;
  for i = 0 to 3 do
    let sql = sprintf "SYNTACTICALLY INCORRECT SQL STATEMENT" in
    printf "%d %s\n%!" i sql;
    try
      match exec db sql ~cb:(fun _ _ -> print_endline "???") with
      | Rc.ERROR -> printf "Identified error: %s\n" (errmsg db)
      | _ -> assert false
    with xcp -> print_endline (Printexc.to_string xcp)
  done;
  for i = 0 to 3 do
    let sql = sprintf "INSERT INTO tbl%d VALUES ('a', 3, 3.14)" i in
    printf "%d %s\n%!" i sql;
    try
      match exec db sql ~cb:(fun _ _ -> print_endline "???") with
      | Rc.OK -> printf "Inserted %d rows\n%!" (changes db)
      | _ -> assert false
    with xcp -> print_endline (Printexc.to_string xcp)
  done;
  let sql = sprintf "SELECT * FROM tbl0" in
  for _i = 0 to 3 do
    try
      print_endline "TESTING!";
      match
        exec db sql ~cb:(fun _ _ ->
            print_endline "FOUND!";
            raise Dummy)
      with
      | Rc.OK -> print_endline "OK"
      | _ -> assert false
    with xcp -> print_endline (Printexc.to_string xcp)
  done;
  true
