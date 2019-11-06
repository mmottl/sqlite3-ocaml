(**************************************************************************)
(*  Copyright (c) 2003 Christian Szegedy <csdontspam871@metamatix.org>    *)
(*                                                                        *)
(*  Copyright (c) 2007 Jane Street Holding, LLC                           *)
(*                     Author: Markus Mottl <markus.mottl@gmail.com>      *)
(*                                                                        *)
(*  Permission is hereby granted, free of charge, to any person           *)
(*  obtaining a copy of this software and associated documentation files  *)
(*  (the "Software"), to deal in the Software without restriction,        *)
(*  including without limitation the rights to use, copy, modify, merge,  *)
(*  publish, distribute, sublicense, and/or sell copies of the Software,  *)
(*  and to permit persons to whom the Software is furnished to do so,     *)
(*  subject to the following conditions:                                  *)
(*                                                                        *)
(*  The above copyright notice and this permission notice shall be        *)
(*  included in all copies or substantial portions of the Software.       *)
(*                                                                        *)
(*  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,       *)
(*  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES       *)
(*  OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND              *)
(*  NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS   *)
(*  BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN    *)
(*  ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN     *)
(*  CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE      *)
(*  SOFTWARE.                                                             *)
(**************************************************************************)

open Printf

exception InternalError of string
exception Error of string
exception RangeError of int * int
exception DataTypeError of string
exception SqliteError of string

type db
type stmt

module Rc = struct
  type unknown

  external int_of_unknown : unknown -> int = "%identity"

  type t =
    | OK
    | ERROR
    | INTERNAL
    | PERM
    | ABORT
    | BUSY
    | LOCKED
    | NOMEM
    | READONLY
    | INTERRUPT
    | IOERR
    | CORRUPT
    | NOTFOUND
    | FULL
    | CANTOPEN
    | PROTOCOL
    | EMPTY
    | SCHEMA
    | TOOBIG
    | CONSTRAINT
    | MISMATCH
    | MISUSE
    | NOFLS
    | AUTH
    | FORMAT
    | RANGE
    | NOTADB
    | ROW
    | DONE
    | UNKNOWN of unknown

  let to_string = function
    | OK -> "OK"
    | ERROR -> "ERROR"
    | INTERNAL -> "INTERNAL"
    | PERM -> "PERM"
    | ABORT  -> "ABORT"
    | BUSY  -> "BUSY"
    | LOCKED -> "LOCKED"
    | NOMEM -> "NOMEM"
    | READONLY -> "READONLY"
    | INTERRUPT -> "INTERRUPT"
    | IOERR -> "IOERR"
    | CORRUPT -> "CORRUPT"
    | NOTFOUND -> "NOTFOUND"
    | FULL -> "FULL"
    | CANTOPEN -> "CANTOPEN"
    | PROTOCOL -> "PROTOCOL"
    | EMPTY -> "EMPTY"
    | SCHEMA -> "SCHEMA"
    | TOOBIG -> "TOOBIG"
    | CONSTRAINT -> "CONSTRAINT"
    | MISMATCH -> "MISMATCH"
    | MISUSE -> "MISUSE"
    | NOFLS -> "NOLFS"
    | AUTH -> "AUTH"
    | FORMAT -> "FORMAT"
    | RANGE -> "RANGE"
    | NOTADB -> "NOTADB"
    | ROW -> "ROW"
    | DONE -> "DONE"
    | UNKNOWN n -> sprintf "UNKNOWN %d" (int_of_unknown n)

  let check = function
    | OK | DONE -> ()
    | err -> raise (SqliteError (to_string err))
end  (* Rc *)

module Data = struct
  type t =
    | NONE
    | NULL
    | INT of int64
    | FLOAT of float
    | TEXT of string
    | BLOB of string

  let opt_text = function Some s -> TEXT s | None -> NULL
  let opt_int = function Some n -> INT (Int64.of_int n) | None -> NULL
  let opt_int64 = function Some n -> INT n | None -> NULL
  let opt_float = function Some n -> FLOAT n | None -> NULL

  let opt_bool = function
    | Some false -> INT Int64.zero
    | Some true -> INT Int64.one
    | None -> NULL


  (* Exception-based type conversion *)

  let to_string_debug = function
    | NONE -> "NONE"
    | NULL -> "NULL"
    | INT i -> sprintf "INT <%Ld>" i
    | FLOAT f -> sprintf "FLOAT <%f>" f
    | TEXT t -> sprintf "TEXT <%S>" t
    | BLOB b -> sprintf "BLOB <%d>" (String.length b)

  let data_type_error tp data =
    let got = to_string_debug data in
    raise (DataTypeError (Printf.sprintf "Expected %s but got %s" tp got))

  let to_string_exn = function
    | TEXT s | BLOB s -> s
    | data -> data_type_error "TEXT or BLOB" data

  let min_int_as_int64 = Int64.of_int min_int
  let max_int_as_int64 = Int64.of_int max_int

  let safe_get_int n =
    if n > max_int_as_int64 then
      failwith (Printf.sprintf "Sqlite3.Data.safe_get_int: overflow: %Ld" n)
    else if n < min_int_as_int64 then
      failwith (Printf.sprintf "Sqlite3.Data.safe_get_int: underflow: %Ld" n)
    else Int64.to_int n

  let to_int_exn = function
    | INT n -> safe_get_int n
    | data -> data_type_error "INT" data

  let to_int64_exn = function
    | INT n -> n
    | data -> data_type_error "INT" data

  let to_float_exn = function
    | FLOAT n -> n
    | data -> data_type_error "FLOAT" data

  let to_bool_exn = function
    | INT 0L -> false
    | INT 1L -> true
    | data -> data_type_error "INT 0L/1L" data


  (* Option-based type conversion *)

  let to_string = function TEXT s | BLOB s -> Some s | _ -> None
  let to_int = function INT n -> Some (safe_get_int n) | _ -> None
  let to_int64 = function INT n -> Some n | _ -> None
  let to_float = function FLOAT n -> Some n | _ -> None

  let to_bool = function
    | INT 0L -> Some false
    | INT 1L -> Some true
    | _ -> None

  (* Simplified string coercion *)
  let to_string_coerce = function
    | NONE | NULL -> ""
    | INT n -> Int64.to_string n
    | FLOAT n -> Float.to_string n
    | TEXT t | BLOB t -> t
end  (* Data *)

type header = string
type headers = header array
type row = string option array
type row_not_null = string array

module Mode = struct
  type t = Read_write_create | Read_write | Read_only

  let lift = function
    | None -> Read_write_create
    | Some `READONLY -> Read_only
    | Some `NO_CREATE -> Read_write
end  (* Mode *)

module Mut = struct
  type t = NOTHING | NO | FULL

  let lift = function None -> NOTHING | Some `NO -> NO | Some `FULL -> FULL
end  (* Mut *)

module Cache = struct
  type t = NOTHING | SHARED | PRIVATE

  let lift =
    function None -> NOTHING | Some `SHARED -> SHARED | Some `PRIVATE -> PRIVATE
end  (* Cache *)

external sqlite_version : unit -> int
  = "caml_sqlite3_version_bc" "caml_sqlite3_version"

external sqlite_version_info : unit -> string
  = "caml_sqlite3_version_str_bc" "caml_sqlite3_version_str"

external db_open :
  mode : Mode.t -> uri : bool -> memory : bool ->
  mutex : Mut.t -> cache : Cache.t ->
  ?vfs : string -> string -> db
  = "caml_sqlite3_open_bc" "caml_sqlite3_open"

let db_open ?mode ?(uri = false) ?(memory = false) ?mutex ?cache ?vfs name =
  let mode = Mode.lift mode in
  let mutex = Mut.lift mutex in
  let cache = Cache.lift cache in
  db_open ~mode ~uri ~memory ~mutex ~cache ?vfs name

external db_close : db -> bool = "caml_sqlite3_close"

external errcode : db -> Rc.t = "caml_sqlite3_errcode"
external errmsg : db -> string = "caml_sqlite3_errmsg"

external last_insert_rowid : db -> (int64 [@unboxed])
  = "caml_sqlite3_last_insert_rowid_bc" "caml_sqlite3_last_insert_rowid"
  [@@noalloc]

external exec :
  db -> ?cb : (string option array -> headers -> unit) -> string -> Rc.t
  = "caml_sqlite3_exec"

external exec_no_headers :
  db -> cb : (string option array -> unit) -> string -> Rc.t
  = "caml_sqlite3_exec_no_headers"

external exec_not_null :
  db -> cb : (string array -> headers -> unit) -> string -> Rc.t
  = "caml_sqlite3_exec_not_null"

external exec_not_null_no_headers :
  db -> cb : (string array -> unit) -> string -> Rc.t
  = "caml_sqlite3_exec_not_null_no_headers"

external changes : db -> (int [@untagged])
  = "caml_sqlite3_changes_bc" "caml_sqlite3_changes"

external prepare : db -> string -> stmt = "caml_sqlite3_prepare"
external prepare_tail : stmt -> stmt option = "caml_sqlite3_prepare_tail"
external recompile : stmt -> unit = "caml_sqlite3_recompile"

external step : stmt -> Rc.t = "caml_sqlite3_step"
external reset : stmt -> Rc.t = "caml_sqlite3_stmt_reset"

let prepare_or_reset db opt_stmt_ref sql =
  match !opt_stmt_ref with
  | Some stmt -> reset stmt |> Rc.check; stmt
  | None ->
      let stmt = prepare db sql in
      opt_stmt_ref := Some stmt;
      stmt

external sleep : (int [@untagged]) -> (int [@untagged])
  = "caml_sqlite3_sleep_bc" "caml_sqlite3_sleep"

external finalize : stmt -> Rc.t = "caml_sqlite3_stmt_finalize"

external data_count : stmt -> (int [@untagged])
  = "caml_sqlite3_data_count_bc" "caml_sqlite3_data_count"

external column_count : stmt -> (int [@untagged])
  = "caml_sqlite3_column_count_bc" "caml_sqlite3_column_count"

external column_blob :
  stmt -> (int [@untagged]) -> string option
  = "caml_sqlite3_column_blob_bc" "caml_sqlite3_column_blob"

external column : stmt -> (int [@untagged]) -> Data.t
  = "caml_sqlite3_column_bc" "caml_sqlite3_column"

let column_to_string stmt num = column stmt num |> Data.to_string
let column_to_int stmt num = column stmt num |> Data.to_int
let column_to_int64 stmt num = column stmt num |> Data.to_int64
let column_to_float stmt num = column stmt num |> Data.to_float
let column_to_bool stmt num = column stmt num |> Data.to_bool

external column_name : stmt -> (int [@untagged]) -> string
  = "caml_sqlite3_column_name_bc" "caml_sqlite3_column_name"

external column_decltype :
  stmt -> (int [@untagged]) -> string option
  = "caml_sqlite3_column_decltype_bc" "caml_sqlite3_column_decltype"

external bind : stmt -> (int [@untagged]) -> Data.t -> Rc.t
  = "caml_sqlite3_bind_bc" "caml_sqlite3_bind"

external bind_parameter_count : stmt -> (int [@untagged])
  = "caml_sqlite3_bind_parameter_count_bc" "caml_sqlite3_bind_parameter_count"

external bind_parameter_name :
  stmt -> (int [@untagged]) -> string option
  = "caml_sqlite3_bind_parameter_name_bc" "caml_sqlite3_bind_parameter_name"

external bind_parameter_index :
  stmt -> string -> (int [@untagged])
  = "caml_sqlite3_bind_parameter_index_bc" "caml_sqlite3_bind_parameter_index"

external clear_bindings : stmt -> Rc.t = "caml_sqlite3_clear_bindings"

external busy_timeout : db -> (int [@untagged]) -> unit
  = "caml_sqlite3_busy_timeout_bc" "caml_sqlite3_busy_timeout"

external enable_load_extension :
  db -> bool -> bool = "caml_sqlite3_enable_load_extension"

let row_blobs stmt = Array.init (data_count stmt) (column_blob stmt)
let row_data stmt = Array.init (data_count stmt) (column stmt)
let row_names stmt = Array.init (data_count stmt) (column_name stmt)
let row_decltypes stmt = Array.init (data_count stmt) (column_decltype stmt)


(* Function registration *)

external create_function :
  db -> string -> (int [@untagged]) -> (Data.t array -> Data.t) -> unit
  = "caml_sqlite3_create_function_bc" "caml_sqlite3_create_function"

let create_funN db name f = create_function db name (-1) f
let create_fun0 db name f = create_function db name 0 (fun _ -> f ())
let create_fun1 db name f = create_function db name 1 (fun args -> f args.(0))

let create_fun2 db name f =
  create_function db name 2 (fun args -> f args.(0) args.(1))

let create_fun3 db name f =
  create_function db name 3 (fun args -> f args.(0) args.(1) args.(2))

external delete_function : db -> string -> unit = "caml_sqlite3_delete_function"

module Aggregate = struct
  external create_function :
    db -> string -> (int [@untagged]) ->
    'a -> ('a -> Data.t array -> 'a) -> ('a -> Data.t array -> 'a) option ->
    ('a -> Data.t) option -> ('a -> Data.t) -> unit =
    "caml_sqlite3_create_aggregate_function_bc"
    "caml_sqlite3_create_aggregate_function"

  let create_funN ?inverse ?value db name ~init ~step ~final =
    create_function db name (-1) init step inverse value final

  let create_fun0 ?inverse ?value db name ~init ~step ~final =
    create_function db name 0 init
      (fun acc _ -> step acc)
      (match inverse with
       | Some inv -> Some (fun acc _ -> inv acc)
       | None -> None)
      value final

  let create_fun1 ?inverse ?value db name ~init ~step ~final =
    create_function db name 1 init
      (fun acc args -> step acc args.(0))
      (match inverse with
       | Some inv -> Some (fun acc args -> inv acc args.(0))
       | None -> None)
      value final

  let create_fun2 ?inverse ?value db name ~init ~step ~final =
    create_function db name 2 init
      (fun acc args -> step acc args.(0) args.(1))
      (match inverse with
       | Some inv -> Some (fun acc args -> inv acc args.(0) args.(1))
       | None -> None)
      value final

  let create_fun3 ?inverse ?value db name ~init ~step ~final =
    create_function db name 3 init
      (fun acc args -> step acc args.(0) args.(1) args.(2))
      (match inverse with
       | Some inv -> Some (fun acc args -> inv acc args.(0) args.(1) args.(2))
       | None -> None)
      value final
end  (* Aggregate *)

module Backup = struct
  type t

  external init :
    dst : db -> dst_name : string ->
    src : db -> src_name : string -> t = "caml_sqlite3_backup_init"

  external step : t -> (int [@untagged]) -> Rc.t
    = "caml_sqlite3_backup_step_bc" "caml_sqlite3_backup_step"

  external finish : t -> Rc.t = "caml_sqlite3_backup_finish"

  external remaining : t -> (int [@untagged])
    = "caml_sqlite3_backup_remaining_bc" "caml_sqlite3_backup_remaining"
    [@@noalloc]

  external pagecount : t -> (int [@untagged])
    = "caml_sqlite3_backup_pagecount_bc" "caml_sqlite3_backup_pagecount"
    [@@noalloc]
end  (* Backup *)

(* Initialisation *)

external init : unit -> unit = "caml_sqlite3_init"
external cleanup : unit -> unit = "caml_sqlite3_cleanup"

let () =
  Callback.register_exception "Sqlite3.InternalError" (InternalError "");
  Callback.register_exception "Sqlite3.Error" (Error "");
  Callback.register_exception "Sqlite3.RangeError" (RangeError (0, 0));
  at_exit cleanup;
  init ()
