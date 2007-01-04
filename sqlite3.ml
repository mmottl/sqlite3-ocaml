(**************************************************************************)
(*  Copyright (c) 2003 Christian Szegedy <csdontspam871@metamatix.org>    *)
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

exception Sqlite3_error of string

exception Sqlite3_range_error of (int * int)

let _ = Callback.register_exception "sqlite3 error"        
    (Sqlite3_error    "Registering Callback")

let _ = Callback.register_exception "sqlite3 range error"  
    (Sqlite3_range_error (0,0))

type db
type stmt

type rc =
   RC_ok
 | RC_error
 | RC_internal
 | RC_perm
 | RC_abort
 | RC_busy
 | RC_locked
 | RC_nomem
 | RC_readonly
 | RC_interrupt
 | RC_ioerr
 | RC_corrupt
 | RC_notfound
 | RC_full
 | RC_cantopen
 | RC_protocol
 | RC_empty
 | RC_schema
 | RC_toobig
 | RC_constraint
 | RC_mismatch
 | RC_misuse 
 | RC_nofls
 | RC_auth
 | RC_format
 | RC_range
 | RC_notadb
 | RC_row
 | RC_done

exception Sqlite_error_with_rc of (string * rc)

type data = 
  | Data_none
  | Data_null
  | Data_int    of int64
  | Data_float  of float
  | Data_text   of string
  | Data_blob   of string

type header = string array

type row   = data array

type table = row array

external db_open              : string -> db                = "caml_sqlite3_open"
external db_close             : db -> unit                  = "caml_sqlite3_close"

external errcode              : db -> rc                    = "caml_sqlite3_errcode"
external errmsg               : db -> string                = "caml_sqlite3_errmsg"
external last_insert_rowid    : db -> int64                 = "caml_sqlite3_last_insert_rowid"

external exec                 : db -> string -> 
  (string option array -> header -> unit) -> rc             = "caml_sqlite3_exec"
(* external exec_ignore : db -> string -> (unit -> unit) -> rc = "caml_sqlite3_exec_ignore" *)

external prepare              : db   -> string -> stmt      = "caml_sqlite3_prepare"
external prepare_tail         : stmt -> stmt option         = "caml_sqlite3_prepare_tail"
external recompile            : stmt -> unit                = "caml_sqlite3_recompile"

external step                 : stmt -> rc                  = "caml_sqlite3_step"
external reset                : stmt -> rc                  = "caml_sqlite3_stmt_reset"
external finalize             : stmt -> rc                  = "caml_sqlite3_stmt_finalize"
external expired              : stmt -> bool                = "caml_sqlite3_expired"

external data_count           : stmt -> int                 = "caml_sqlite3_data_count"
external column               : stmt -> int -> data         = "caml_sqlite3_column"
external column_name          : stmt -> int -> string       = "caml_sqlite3_column_name"
external column_decltype      : stmt -> int -> string       = "caml_sqlite3_column_decltype"

external bind                 : stmt -> int -> data -> rc   = "caml_sqlite3_bind"
external bind_parameter_count : stmt -> int                 = "caml_sqlite3_bind_parameter_count"
external bind_parameter_name  : stmt -> int -> string option= "caml_sqlite3_bind_parameter_name"
external bind_parameter_index : stmt -> string -> int       = "caml_sqlite3_bind_parameter_index"
external transfer_bindings    : stmt -> stmt -> rc          = "caml_sqlite3_transfer_bindings"

(* external sleep   : int -> unit  = "caml_sqlite3_sleep" *)
(* clear_bindings   : stmt -> rc   = "caml_sqlite3_clear_bindings" *)


let string_of_data data = 
  match data with
  | Data_none    -> "none"
  | Data_null    -> "null"   
  | Data_int   i -> "int <"           ^ Int64.to_string i ^ ">"
  | Data_float f -> "float <"         ^ string_of_float f ^ ">"
  | Data_text  t -> "text \""         ^ t ^ "\""
  | Data_blob  b -> "blob of length " ^ string_of_int (String.length b)
                                          
let string_of_rc rc = 
  match rc with
  | RC_ok -> "ok"
  | RC_error -> "error"
  | RC_internal -> "internal"
  | RC_perm -> "perm"
  | RC_abort  -> "abort"
  | RC_busy  -> "busy"
  | RC_locked -> "locked"
  | RC_nomem -> "nomem"
  | RC_readonly -> "readonly"
  | RC_interrupt -> "interrupt"
  | RC_ioerr -> "ioerr"
  | RC_corrupt -> "corrupt"
  | RC_notfound -> "notfound"
  | RC_full -> "full"
  | RC_cantopen -> "cantopen"
  | RC_protocol -> "protocol"
  | RC_empty -> "empty"
  | RC_schema -> "schema"
  | RC_toobig -> "toobig"
  | RC_constraint -> "constraint"
  | RC_mismatch -> "mismatch"
  | RC_misuse -> "misuse"
  | RC_nofls -> "nolfs"
  | RC_auth -> "auth"
  | RC_format -> "format"
  | RC_range -> "range"
  | RC_notadb -> "notadb"
  | RC_row -> "row"
  | RC_done -> "done"
  | _ -> "unknown"

let row_data  stmt      = Array.init (data_count stmt) (column stmt) 

let row_names stmt      = Array.init (data_count stmt) (column_name stmt) 

let row_decltypes stmt  = Array.init (data_count stmt) (column_decltype stmt) 

let exec_sql f db sql = 
  let x = ref (Some (prepare db sql) ) in
  while 
    ( match !x with
    | Some s ->
        while step s = RC_row do
          f s
        done;
        x := prepare_tail s;
        true
    | None -> false ) do () done
