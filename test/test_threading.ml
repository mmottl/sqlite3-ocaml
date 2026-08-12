open Sqlite3

let assert_ok rc = assert (rc = Rc.OK)

let with_lock mutex f =
  Mutex.lock mutex;
  Fun.protect f ~finally:(fun () -> Mutex.unlock mutex)

let run_prepare_finalize_during_exec_callback () =
  let db = db_open ~mutex:`FULL "t_threading" in
  assert_ok (exec db "DROP TABLE IF EXISTS thread_deadlock");
  assert_ok (exec db "CREATE TABLE thread_deadlock (x INTEGER)");
  assert_ok (exec db "INSERT INTO thread_deadlock VALUES (1)");

  let mutex = Mutex.create () in
  let callback_entered = Condition.create () in
  let callback_release = Condition.create () in
  let in_callback = ref false in
  let may_return = ref false in
  let worker_failure = ref None in

  let record_failure exn =
    with_lock mutex (fun () ->
        worker_failure := Some (Printexc.to_string exn);
        Condition.broadcast callback_entered;
        Condition.broadcast callback_release)
  in

  let exec_thread =
    Thread.create
      (fun () ->
        try
          assert_ok
            (exec db
               ~cb:(fun _row _headers ->
                 with_lock mutex (fun () ->
                     in_callback := true;
                     Condition.broadcast callback_entered;
                     while not !may_return do
                       Condition.wait callback_release mutex
                     done))
               "SELECT x FROM thread_deadlock")
        with exn -> record_failure exn)
      ()
  in

  with_lock mutex (fun () ->
      while (not !in_callback) && !worker_failure = None do
        Condition.wait callback_entered mutex
      done);

  (match !worker_failure with Some msg -> failwith msg | None -> ());

  let worker_thread =
    Thread.create
      (fun () ->
        try
          let stmt = prepare db "SELECT 42" in
          assert_ok (finalize stmt)
        with exn -> record_failure exn)
      ()
  in

  Thread.delay 0.05;
  with_lock mutex (fun () ->
      may_return := true;
      Condition.broadcast callback_release);

  Thread.join exec_thread;
  Thread.join worker_thread;
  (match !worker_failure with Some msg -> failwith msg | None -> ());
  assert (db_close db)

let run_finalize_remains_rooted () =
  let db = db_open ~mutex:`FULL ":memory:" in
  assert_ok (exec db "CREATE TABLE t (x INTEGER)");
  assert_ok (exec db "INSERT INTO t VALUES (1)");

  let stmt_slot = ref (Some (prepare db "SELECT 42")) in
  let mutex = Mutex.create () in
  let callback_entered = Condition.create () in
  let callback_release = Condition.create () in
  let finalize_started = Condition.create () in
  let in_callback = ref false in
  let may_return = ref false in
  let is_finalizing = ref false in

  let exec_thread =
    Thread.create
      (fun () ->
        assert_ok
          (exec db
             ~cb:(fun _row _headers ->
               with_lock mutex (fun () ->
                   in_callback := true;
                   Condition.broadcast callback_entered;
                   while not !may_return do
                     Condition.wait callback_release mutex
                   done))
             "SELECT x FROM t"))
      ()
  in

  with_lock mutex (fun () ->
      while not !in_callback do
        Condition.wait callback_entered mutex
      done);

  let finalize_thread =
    Thread.create
      (fun () ->
        match !stmt_slot with
        | None -> assert false
        | Some stmt ->
            stmt_slot := None;
            with_lock mutex (fun () ->
                is_finalizing := true;
                Condition.broadcast finalize_started);
            assert_ok (finalize stmt))
      ()
  in

  with_lock mutex (fun () ->
      while not !is_finalizing do
        Condition.wait finalize_started mutex
      done);
  Thread.delay 0.05;
  Gc.minor ();

  with_lock mutex (fun () ->
      may_return := true;
      Condition.broadcast callback_release);

  Thread.join exec_thread;
  Thread.join finalize_thread;
  assert (db_close db)

let wait_for_child pid timeout_s =
  let deadline = Unix.gettimeofday () +. timeout_s in
  let rec loop () =
    match Unix.waitpid [ Unix.WNOHANG ] pid with
    | 0, _ ->
        if Unix.gettimeofday () >= deadline then (
          Unix.kill pid Sys.sigkill;
          ignore (Unix.waitpid [] pid);
          failwith "child process timed out")
        else (
          Thread.delay 0.01;
          loop ())
    | _, Unix.WEXITED 0 -> ()
    | _, Unix.WEXITED code ->
        failwith (Printf.sprintf "child process exited with code %d" code)
    | _, Unix.WSIGNALED signal ->
        failwith (Printf.sprintf "child process killed by signal %d" signal)
    | _, Unix.WSTOPPED signal ->
        failwith (Printf.sprintf "child process stopped by signal %d" signal)
  in
  loop ()

let%test_unit "test_threading_exec_prepare_finalize" =
  match Unix.fork () with
  | 0 -> (
      try
        run_prepare_finalize_during_exec_callback ();
        exit 0
      with exn ->
        prerr_endline (Printexc.to_string exn);
        exit 1)
  | pid -> wait_for_child pid 3.0

let%test_unit "test_threading_finalize_remains_rooted" =
  match Unix.fork () with
  | 0 -> (
      try
        run_finalize_remains_rooted ();
        exit 0
      with exn ->
        prerr_endline (Printexc.to_string exn);
        exit 1)
  | pid -> wait_for_child pid 3.0
