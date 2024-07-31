open Sqlite3

exception This_function_always_fails

(* Tests our ability to raise an exception from a user-defined function *)
let%test "test_error" =
  let db = db_open "t_error" in
  create_fun0 db "MYERROR" (fun () -> raise This_function_always_fails);
  let first_test =
    try
      let res = exec db "SELECT MYERROR();" in
      prerr_endline ("Should have thrown an error: " ^ Rc.to_string res);
      false
    with This_function_always_fails ->
      print_endline "Ok";
      true
  in

  (* This pattern shows typical usage *)
  exec db "PRAGMA synchronous = OFF;" |> Rc.check;
  exec db "PRAGMA journal_mode = MEMORY;" |> Rc.check;
  let second_test =
    try
      exec db "THIS SHOULD THROW AN EXCEPTION;; BECAUSE IT IS NOT VALID;;"
      |> Rc.check;
      false
    with SqliteError _ ->
      print_endline "Ok";
      true
  in
  first_test && second_test
