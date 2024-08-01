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

  (* Check the extended error code. *)
  exec db "CREATE TABLE erc1 (x INTEGER UNIQUE NOT NULL CHECK (x > 0))"
  |> Rc.check;
  exec db "CREATE TABLE erc2 (x INTEGER PRIMARY KEY, y REFERENCES erc1(x))"
  |> Rc.check;
  let erc_test expected_erc q =
    let _ = exec db q in
    let erc = extended_errcode_int db in
    if erc = expected_erc then (
      print_endline "Ok";
      true)
    else (
      Printf.eprintf "Expected extended error code %d for %S, got %d.\n%!"
        expected_erc q erc;
      false)
  in

  first_test && second_test
  && erc_test 1299 "INSERT INTO erc1 (x) VALUES (NULL)"
  && erc_test 275 "INSERT INTO erc1 (x) VALUES (0)"
  && erc_test 2067 "INSERT INTO erc1 (x) VALUES (1), (1)"
  && erc_test 1555 "INSERT INTO erc2 (x) VALUES (1), (1)"
