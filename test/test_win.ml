open Sqlite3

let%test "test_window" =
  Printf.printf "Using version %s\n" (sqlite_version_info ());
  if sqlite_version () >= 3025000 then begin
    let db = db_open "t_fun" in
    let getval = (fun p -> Data.FLOAT p) in
    Aggregate.create_fun1 db "product" ~init:1.0
      ~step:(fun p v ->  p *. (Data.to_float_exn v))
      ~inverse:(fun p v -> p /. (Data.to_float_exn v))
      ~value:getval
      ~final:getval;
    let s =
      prepare db "\
        WITH cte(id, num) AS (VALUES (0,2.0),(1,3.0),(2,4.0),(3,5.0),(4,6.0)) \
        SELECT id, product(num) \
        OVER (ORDER BY id ROWS BETWEEN 2 PRECEDING AND CURRENT ROW) \
        FROM cte ORDER BY id"
    in
    let expected = [| 2.0; 6.0; 24.0; 60.0; 120.0 |] in
    print_endline "Testing window functions.";
    while step s = Rc.ROW do
      Printf.printf "got %f expected %f\n" (column_double s 1)
        expected.(column_int s 0)
    done
  end else
    prerr_endline "Skipping window function test.";
  true
