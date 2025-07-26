open Sqlite3

let get_all stmt : Data.t array list =
  let f acc row = row :: acc in
  let rc, rows = fold stmt ~f ~init:[] in
  assert (rc = Rc.DONE);
  List.rev rows

let compare_data (lhs : Data.t) (rhs : Data.t) : int =
  let open Data in
  match (lhs, rhs) with
  | NONE, NONE -> 0
  | NULL, NULL -> 0
  | INT x, INT y -> Int64.compare x y
  | FLOAT x, FLOAT y -> Float.compare x y
  | TEXT x, TEXT y -> String.compare x y
  | BLOB x, BLOB y -> String.compare x y
  | _ -> 1

let compare_query_row (lhs : Data.t array) (rhs : Data.t array) =
  let lhs = Array.to_seq lhs in
  let rhs = Array.to_seq rhs in
  Seq.compare compare_data lhs rhs

let%test "can_get_int64_values_out_of_carray" =
  let db = db_open ":memory:" in

  let carray =
    Bigarray.(Array1.of_array int64 c_layout [| 1L; 2L; 10L; 11L; 30L |])
    |> CArray.of_int64_bigarray
  in
  let stmt = prepare db "SELECT value FROM carray($ptr)" in
  assert (Rc.OK = bind_carray stmt 1 carray);
  let expected : Data.t array list =
    [ [| INT 1L |]; [| INT 2L |]; [| INT 10L |]; [| INT 11L |]; [| INT 30L |] ]
  in
  assert (0 = List.compare compare_query_row (get_all stmt) expected);
  true

let%test "carray_lifetime_is_bound_to_stmt" =
  let db = db_open ":memory:" in

  let stmt =
    let carray =
      Bigarray.(Array1.of_array int64 c_layout [| 1L; 2L; 10L; 11L; 30L |])
      |> CArray.of_int64_bigarray
    in
    let stmt = prepare db "SELECT value FROM carray($ptr)" in
    assert (Rc.OK = bind_carray stmt 1 carray);
    stmt
  in
  Gc.full_major ();
  let expected : Data.t array list =
    [ [| INT 1L |]; [| INT 2L |]; [| INT 10L |]; [| INT 11L |]; [| INT 30L |] ]
  in
  assert (0 = List.compare compare_query_row (get_all stmt) expected);
  true

let make_carray size =
  let ba = Bigarray.(Array1.create int64 c_layout size) in
  CArray.of_int64_bigarray ba

let%test "memory_for_bigarray_is_released_when_finalizing_statement" =
  let db = db_open ":memory:" in
  let was_array_collected, stmt =
    let ba = Bigarray.(Array1.create int64 c_layout 10) in
    let was_array_collected = ref false in
    Gc.finalise_last (fun () -> was_array_collected := true) ba;
    let stmt = prepare db "SELECT value FROM carray($ptr)" in
    assert (Rc.OK = bind_carray stmt 1 (CArray.of_int64_bigarray ba));
    (was_array_collected, stmt)
  in
  Gc.full_major ();
  assert (not !was_array_collected);
  Rc.check (finalize stmt);
  Gc.full_major ();
  assert !was_array_collected;
  true
