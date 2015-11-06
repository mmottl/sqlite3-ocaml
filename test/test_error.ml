open Sqlite3

exception This_function_always_fails

(* Tests our ability to raise an exception from a user-defined function *)
let () =
  let db = db_open "t" in
  create_fun0 db "MYERROR" (fun () -> raise This_function_always_fails);
  try
    let res = exec db "SELECT MYERROR();" in
    prerr_endline ("Should have thrown an error: " ^ Rc.to_string res)
  with This_function_always_fails -> print_endline "Ok"
