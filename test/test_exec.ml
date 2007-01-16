open Sqlite3
exception Dummy

let _ = 
  let db = db_open "t" in
  for i = 0 to 10 do
    try 
      let sql = Printf.sprintf "CREATE TABLE tbl%d (a varchar(1),b INTEGER,c FLOAT)" i in
      Printf.printf "%d %s\n%!" i sql;
      let rc = exec db sql (fun _ _ -> print_endline "???") in
      ()
    with xcp -> print_endline (Printexc.to_string xcp)
  done; 
  for i = 0 to 3 do
    let sql = Printf.sprintf "SYNTACTICALLY INCORRECT SQL STATEMENT" in
    Printf.printf "%d %s\n%!" i sql;
    try 
      let rc = exec db sql (fun _ _ -> print_endline "???") in
      ()
    with 
      xcp -> 
        print_endline (Printexc.to_string xcp)
  done;
  for i = 0 to 3 do
    let sql = Printf.sprintf "INSERT INTO tbl%d VALUES ('a',3,3.14)" i in
    Printf.printf "%d %s\n%!" i sql;
    try 
      let rc = exec db sql (fun _ _ -> print_endline "???") in
      ()
    with 
      xcp -> 
        print_endline (Printexc.to_string xcp)
  done;
  
  let sql = Printf.sprintf "SELECT * FROM tbl0" in

  for i = 0 to 3 do
    try 
      print_endline "TESTING!";
      let rc = exec db sql (fun _ _ -> 
        print_endline "FOUND!";
        raise Dummy) in
      print_endline "OK"
    with 
      xcp -> 
        print_endline (Printexc.to_string xcp)
  done
