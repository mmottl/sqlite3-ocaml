open Sqlite3

let () =
  let db = db_open "t" in
  Aggregate.create_fun2 db "STRREPEAT" ~init:[] ~step:(fun l s i  ->
    match s, i with
    | Data.TEXT s, Data.INT i ->
       let suffix = String.make (Int64.to_int i) s.[0] in
       (s ^ suffix) :: l
    | _ -> raise (Error "wrong types to 'STRREPEAT'")) 
    ~final:(fun l -> Data.TEXT (String.concat " | " (List.rev l)));
  let sqls =
    [
      "DROP TABLE IF EXISTS tbl";
      "CREATE TABLE tbl (a varchar(10), a2 varchar(10), b INTEGER, c FLOAT)";
      "INSERT INTO tbl VALUES ('pippo', 'foo', 3, 3.14)";
      "INSERT INTO tbl VALUES ('bar', 'onion', 5, 3.14)";
      "SELECT STRREPEAT(a, b) FROM tbl";
      "SELECT STRREPEAT(a2, b) FROM tbl";
    ]
  in
  List.iter (fun sql ->
      try
        let res =
          exec db sql ~cb:(fun row _ ->
            match row.(0) with
            | Some a -> print_endline a
            | _ -> ())
        in
        match res with
        | Rc.OK -> ()
        | r ->
            prerr_endline (Rc.to_string r);
            prerr_endline (errmsg db)
      with Error s -> prerr_endline s)
   sqls
