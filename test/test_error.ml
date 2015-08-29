open Sqlite3

(* Tests our ability to return an error from a user defined function *)
let _ =
  let db = db_open "t" in
  create_fun0 db "MYERROR" (fun () -> Data.ERROR "This function always errors");
  let res = exec db "SELECT MYERROR();" in
  match res with
  | Rc.ERROR -> print_endline (errmsg db)
  | x -> prerr_endline ("Should have thrown an error: " ^ Rc.to_string x)

(* Insures that we can't bind an error to a query *)
let _ =
  let db = db_open "t" in
  let _ = exec db "CREATE TABLE foo (val text);" in
  let s = Sqlite3.prepare db "INSERT INTO foo values (?);" in
  let res = Sqlite3.bind s 1 (Sqlite3.Data.ERROR "Should be impossible") in
  match res with
  | Rc.ERROR -> print_endline ("Bind threw an error")
  | x -> prerr_endline ("Should have thrown an error: " ^ Rc.to_string x)
