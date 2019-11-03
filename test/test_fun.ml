open Printf
open Sqlite3

let%test "test_fun" =
  let db = db_open "t_fun" in
  create_fun2 db "REGEX" (fun s rex ->
    match rex, s with
    | Data.TEXT rex, Data.BLOB s
    | Data.TEXT rex, Data.TEXT s ->
       let r = Str.regexp rex in
       if Str.string_match r s 0 then Data.INT 1L else Data.INT 0L
    | _ -> raise (Sqlite3.Error "wrong types to 'REGEX'"));
  let sqls =
    [
      "DROP TABLE IF EXISTS tbl";
      "CREATE TABLE tbl (a varchar(10), b INTEGER, c FLOAT)";
      "INSERT INTO tbl VALUES ('pippo', 3, 3.14)";
      "SELECT * FROM tbl where REGEX(a,'^pippo$')";
      "SELECT * FROM tbl where REGEX(a,'^ippo')";
      "SELECT * FROM tbl where REGEX(a,'[^z]*')";
    ]
  in
  List.iter (fun sql ->
      try
        let res =
          exec db sql ~cb:(fun row _ ->
            match row.(0), row.(1), row.(2) with
            | Some a, Some b, Some c -> printf "%s|%s|%s\n" a b c
            | _ -> ())
        in
        match res with
        | Rc.OK -> ()
        | r ->
            prerr_endline (Rc.to_string r);
            prerr_endline (errmsg db)
      with Sqlite3.Error s -> prerr_endline s)
   sqls;
   true
