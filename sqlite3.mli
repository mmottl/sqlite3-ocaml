(**************************************************************************)
(*  Copyright (c) 2005 Christian Szegedy <csdontspam@metamatix.com>       *)
(*  Copyright (c) 2007 Markus Mottl <markus.mottl@gmail.com>              *)
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

(** API to work with Sqlite 3.* databases *)

(** {2 Exceptions} *)

exception InternalError of string

(** [Error] is raised when some SQL operation is called on a
    nonexistent handle and the functions does not return a return code.
    Functions returning return code communicate erros by returning the
    specific code. *)
exception Error of string

(** Range error is raised if some column or bind operation refers to
    a nonexistent column or binding. The first entry of the returned tuple
    is the specified index the second is the limit which was violated. *)
exception RangeError of int * int


(** {2 Datas} *)

(** Database handle. Used to store information regarding an open
    database and error code from the last operation if the function
    implementing that operation takes database handle as a parameter. *)
type db

(** Compiled statement handle. Stores information about  a
    compiled statement created by [prepare] or [prepare_tail] functions.
*)
type stmt

(** Possible error codes from failed or successful operations.*)
module Rc : sig
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
    | UNKNOWN of int32

  val to_string : t -> string
end

(** Data returned by a column query. *)
module Data : sig
  type t =
    | NONE
    | NULL
    | INT of int64
    | FLOAT of float
    | TEXT of string
    | BLOB of string

  val to_string : t -> string
end

(* Header: names of the columns in question. *)
type header = string array

(** {2 General database operations} *)

(** [db_open filename] opens the database file [filename] and
    returns a database handle. *)
external db_open : string -> db = "caml_sqlite3_open"

(** [db_close db] Closes a database file and invalidates
    the handle.  Raises an [SqliteError] exception if invalid handle is
    provided. *)
external db_close : db -> bool = "caml_sqlite3_close"


(** [errcode db] Returns the error code of the last operation.
    Raises an [SqliteError] exception if invalid handle is passed. *)
external errcode : db -> Rc.t = "caml_sqlite3_errcode"

(** [errmsg db] Returns the error message of the last operation.
    Raises an [SqliteError] exception if invalid handle is passed. *)
external errmsg : db -> string = "caml_sqlite3_errmsg"

(** Get the index of the row inserted by the last operation. *)
external last_insert_rowid : db -> int64 = "caml_sqlite3_last_insert_rowid"

(** [exec sql callback] performs an SQL operation. If the operation contains
    query statements then the callback function is called for each matching rows.
    The first parameter is the array of the data in string format. The second
    paramater is the header of the columns: an array of string of the same size
    as the first parameter. *)
external exec : db -> string ->
  (string option array -> header -> unit) -> Rc.t = "caml_sqlite3_exec"

(** {2 Fine grained query operations} *)

(** Compile an SQL statement into bytecode. The statement may be only partially compiled.
    In this case [prepare_tail] can be called on the returned statement to compiled the
    remaining part of the SQL statement.
*)
external prepare : db -> string -> stmt = "caml_sqlite3_prepare"

(** Compile an SQL statement into bytecode. Compile the remaining part of an SQL
    statement. *)
external prepare_tail : stmt -> stmt option = "caml_sqlite3_prepare_tail"

(** Recompile an SQL statement into bytecode. The statement may be only partially compiled.
    In this case [prepare_tail] can be called on the returned statement to compiled the
    remaining part of the SQL statement. Call this function if the statement expires
    due to some schema change.
*)
external recompile : stmt -> unit = "caml_sqlite3_recompile"

(** Perform one step of the query. *)
external step : stmt -> Rc.t = "caml_sqlite3_step"

(** Finalize the statement. After finalization, the only valid usage of the
    statement is to call it in prepare_tail or recompile. *)
external finalize : stmt -> Rc.t = "caml_sqlite3_stmt_finalize"

(** Reset the statement (i.e. start the query from a new, perhaps with
    different bindings.) *)
external reset : stmt -> Rc.t = "caml_sqlite3_stmt_reset"

(** Returns true if the statement has expired. In this case. it may need to be
    recompiled. *)
external expired : stmt -> bool = "caml_sqlite3_expired"

(** {2 Data query} *)

(** Get the number of columns in the result of the last step. *)
external data_count : stmt -> int = "caml_sqlite3_data_count"

(** Get the data in the specified column of the result of the last step. *)
external column : stmt -> int -> Data.t = "caml_sqlite3_column"

(** Get the header of the specified column of the result of the last step. *)
external column_name : stmt -> int -> string = "caml_sqlite3_column_name"

(** Get the declared type of the specified column of the result of the last step. *)
external column_decltype : stmt -> int -> string = "caml_sqlite3_column_decltype"


(** {2 Binding data to the query} *)

(** [bind stmt index data] Binds [data] to the free variable at the position [index] of
    statement [stmt]. Caution: the first variable is has index 1! *)
external bind : stmt -> int -> Data.t -> Rc.t = "caml_sqlite3_bind"

(** Get the number of variable to be bound. *)
external bind_parameter_count : stmt -> int = "caml_sqlite3_bind_parameter_count"

(** Get the parameter name of the free variable at the specified position of the statement. *)
external bind_parameter_name : stmt -> int -> string option= "caml_sqlite3_bind_parameter_name"

(** Lookup the position of by the name of the free variable in the statement. *)
external bind_parameter_index : stmt -> string -> int = "caml_sqlite3_bind_parameter_index"

(** Transfer the bindings of the statement to the other statement (second parameter) *)
external transfer_bindings : stmt -> stmt -> Rc.t = "caml_sqlite3_transfer_bindings"


(* external sleep : int -> unit = "caml_sqlite3_sleep" *)

(* external exec_ignore : db -> string -> (unit -> unit) -> Rc.t = "caml_sqlite3_exec_ignore" *)


(** {2 Pretty printing functions} *)

(** {2 Stepwise query convenience functions} *)

(** Perform a query in a stepwise manner. *)
val exec_sql : (stmt -> unit) -> db -> string -> unit

(** Get all the declared types of the columns of the last query step
    in an array *)
val row_data : stmt -> Data.t array

(** Get all the column names of the last query step in an array. *)
val row_names : stmt -> string array

(** Get all the data of the last query step in an array. *)
val row_decltypes : stmt -> string array
