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

let make_bigarray size = Bigarray.(Array1.create int64 c_layout size)

let evaluate_if_collected obj =
  let was_collected = ref false in
  Gc.finalise_last (fun _ -> was_collected := true) obj;
  was_collected

let%test "memory_for_bigarray_is_released_when_finalizing_statement" =
  let db = db_open ":memory:" in
  let was_array_collected, stmt =
    let ba = make_bigarray 10 in
    let was_array_collected = evaluate_if_collected ba in
    let stmt = prepare db "SELECT value FROM carray($ptr)" in
    assert (Rc.OK = bind_carray stmt 1 (CArray.of_int64_bigarray ba));
    (was_array_collected, stmt)
  in

  (* at first, the bigarray is not collected because it's bound to the
     statement *)
  Gc.full_major ();
  assert (not !was_array_collected);

  (* but after finalizing the statement *)
  Rc.check (finalize stmt);

  (* the array can get collected *)
  Gc.full_major ();
  assert !was_array_collected;
  true

let%test "rebinding carray makes previously-bound carray collectable" =
  let db = db_open ":memory:" in
  let was_array_collected, stmt =
    let ba = make_bigarray 10 in
    let was_array_collected = evaluate_if_collected ba in
    let stmt = prepare db "SELECT value FROM carray($ptr)" in
    assert (Rc.OK = bind_carray stmt 1 (CArray.of_int64_bigarray ba));
    (was_array_collected, stmt)
  in
  (* at first, the bigarray is not collected because it's bound to the
     statement *)
  Gc.full_major ();
  assert (not !was_array_collected);

  (* but then we bind a different carray to the same position in the
     statement *)
  let new_carray = make_bigarray 10 |> CArray.of_int64_bigarray in
  assert (Rc.OK = bind_carray stmt 1 new_carray);

  (* and then the array does get collected *)
  Gc.full_major ();
  assert !was_array_collected;
  true

let make_sql_query_with_n_carrays n =
  let carray_part = List.init n (fun i -> "carray(?) as c" ^ string_of_int i) in
  "SELECT * FROM " ^ String.concat ", " carray_part

let%test "rebinding_clears_only_one_of_the_bigarrays" =
  let n = 3 in
  let db = db_open ":memory:" in
  let collections, stmt =
    let arrays = List.init n (fun _ -> make_bigarray 10) in
    let collections = List.map evaluate_if_collected arrays in
    let stmt = prepare db (make_sql_query_with_n_carrays n) in
    List.iteri
      (fun i array ->
        assert (
          Rc.OK = bind_carray stmt (i + 1) (CArray.of_int64_bigarray array)))
      arrays;
    (collections, stmt)
  in

  (* at first, no carray gets collected (they are all bound to the statement) *)
  Gc.full_major ();
  assert (List.for_all (fun was_collected -> not !was_collected) collections);

  (* when we rebind parameter 1 *)
  assert (
    Rc.OK = bind_carray stmt 1 (CArray.of_int64_bigarray (make_bigarray 10)));

  (* only that one gets collected *)
  Gc.full_major ();
  assert !(List.nth collections 0);
  assert (not !(List.nth collections 1));
  assert (not !(List.nth collections 2));

  (* when we rebind parameter 3 *)
  assert (
    Rc.OK = bind_carray stmt 3 (CArray.of_int64_bigarray (make_bigarray 10)));

  (* that one gets collected too *)
  Gc.full_major ();
  assert !(List.nth collections 2);
  assert (not !(List.nth collections 1));

  true
