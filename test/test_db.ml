open Sqlite3

let _ = 
  for i = 1 to 1000 do
    Printf.printf "->%d\n%!" i;
    ignore (db_open "t");
    Printf.printf "<-%d\n%!" i;
  done
