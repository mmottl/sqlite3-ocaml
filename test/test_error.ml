open Sqlite3

(* Tests our ability to return an error from a user defined function *)
let () =
  let db = db_open "t" in
  create_fun0 db "MYERROR" (fun () -> failwith "This function always errors");
  let res = exec db "SELECT MYERROR();" in
  prerr_endline ("Should have thrown an error: " ^ Rc.to_string res)
