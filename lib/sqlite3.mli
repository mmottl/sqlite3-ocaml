(**************************************************************************)
(*  Copyright © 2010-     Markus Mottl                                    *)
(*  Copyright © 2007-2010 Jane Street Holding, LLC                        *)
(*  Copyright © 2005-2007 Christian Szegedy                               *)
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

(** API for Sqlite 3.* databases *)

(** {2 Exceptions} *)

exception InternalError of string
(** [InternalError reason] is raised when the bindings detect an
    unknown/unsupported situation. *)

exception Error of string
(** [Error reason] is raised when some SQL operation is called on a nonexistent
    handle and the functions does not return a return code, or if there is no
    error code corresponding to this error. Functions returning return codes
    communicate errors by returning the specific error code. *)

exception RangeError of int * int
(** [RangeError (index, maximum)] is raised if some column or bind operation
    refers to a nonexistent column or binding. The first entry of the returned
    tuple is the specified index, the second is the limit which was violated. *)

exception DataTypeError of string
(** [DataTypeError msg] is raised when you attempt to convert a [Data.t]
    structure to an object via an invalid conversion. *)

exception SqliteError of string
(** [SqliteError err_msg] is raised after calling [Rc.check] on a return code
    that does not indicate success. *)

(** {2 Library Information} *)

val sqlite_version : unit -> int
(** [sqlite_version ()]
    @return
      the version of the SQLite3 library being used, in format [3XXXYYY] where
      [XXX] is the minor version and [YYY] is the patch level. For example,
      [3030001] for 3.30.1. *)

val sqlite_version_info : unit -> string
(** [sqlite_version_info ()]
    @return
      the version of the SQLite3 library being used in a human-readable string.
*)

(** {2 Types} *)

type db
(** Database handle. Used to store information regarding open databases and the
    error code from the last operation if the function implementing that
    operation takes a database handle as a parameter.

    NOTE: database handles are closed (see {!db_close}) automatically when they
    are reclaimed by the GC unless they have already been closed earlier by the
    user. It is good practice to manually close database handles to free
    resources as quickly as possible.

    @see <https://sqlite.org/threadsafe.html>
      about thread safety when accessing database handles and also consider
      using the [mutex] flag with {!db_open} if necessary. *)

type stmt
(** Compiled statement handle. Stores information about compiled statements
    created by the [prepare] or [prepare_tail] functions.

    @see <https://sqlite.org/threadsafe.html>
      about thread safety when accessing statement handles. *)

type header = string
(** Type of name of a column returned by queries. *)

type headers = header array
(** Type of names of columns returned by queries. *)

type row = string option array
(** Type of row data (with potential NULL-values) *)

type row_not_null = string array
(** Type of row data (without NULL-values) *)

(** {2 Return codes} *)

module Rc : sig
  type unknown
  (** Type of unknown return codes *)

  val int_of_unknown : unknown -> int
  (** [int_of_unknown n] converts unknown return code [rc] to an integer. *)

  (** Type of return codes from failed or successful operations. *)
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

  val to_string : t -> string
  (** [to_string rc] converts return code [rc] to a string. *)

  val check : t -> unit
  (** [check rc] raises an exception if [rc] does not correspond to a return
      code indicating success. *)

  val is_success : t -> bool
  (** [is_success rc]
      @return
        [true] if [rc] indicates success ([OK] or [DONE]), [false] otherwise. *)
end

(** {2 Column data types} *)

module Data : sig
  (** Type of columns *)
  type t =
    | NONE
    | NULL
    | INT of int64
    | FLOAT of float
    | TEXT of string
    | BLOB of string

  val opt_text : string option -> t
  (** [opt_text value] converts [value] to a [Data.t] [TEXT] value, converting
      [None] to SQLite [NULL]. *)

  val opt_int : int option -> t
  (** [opt_int value] converts [value] to a [Data.t] [INT] value, converting
      [None] to SQLite [NULL]. *)

  val opt_nativeint : nativeint option -> t
  (** [opt_nativeint value] converts [value] to a [Data.t] [INT] value,
      converting [None] to SQLite [NULL]. *)

  val opt_int32 : int32 option -> t
  (** [opt_int32 value] converts [value] to a [Data.t] [INT] value, converting
      [None] to SQLite [NULL]. *)

  val opt_int64 : int64 option -> t
  (** [opt_int64 value] converts [value] to a [Data.t] [INT] value, converting
      [None] to SQLite [NULL]. *)

  val opt_float : float option -> t
  (** [opt_float value] converts [value] to a [Data.t] [FLOAT] value, converting
      [None] to SQLite [NULL]. *)

  val opt_bool : bool option -> t
  (** [opt_bool value] converts [value] to a [Data.t] [INT] value, converting
      [None] to SQLite [NULL]. *)

  val to_string_exn : t -> string
  (** [to_string_exn data] converts [TEXT] and [BLOB] [data] to a string.

      @raise DataTypeError if [data] is invalid. *)

  val to_int_exn : t -> int
  (** [to_int_exn data] converts [INT] [data] to an int.

      @raise DataTypeError if [data] is invalid.
      @raise Failure if the integer conversion over- or underflows. *)

  val to_nativeint_exn : t -> nativeint
  (** [to_nativeint_exn data] converts [INT] [data] to a nativeint.

      @raise DataTypeError if [data] is invalid.
      @raise Failure if the integer conversion over- or underflows. *)

  val to_int32_exn : t -> int32
  (** [to_int32_exn data] converts [INT] [data] to an int32.

      @raise DataTypeError if [data] is invalid.
      @raise Failure if the integer conversion over- or underflows. *)

  val to_int64_exn : t -> int64
  (** [to_int64_exn data] converts [INT] [data] to an int64.

      @raise DataTypeError if [data] is invalid. *)

  val to_float_exn : t -> float
  (** [to_float_exn data] converts [FLOAT] [data] to a float.

      @raise DataTypeError if [data] is invalid. *)

  val to_bool_exn : t -> bool
  (** [to_bool_exn data] converts [INT] [data] to a bool.

      @raise DataTypeError if [data] is invalid. *)

  val to_string : t -> string option
  (** [to_string data] converts [data] to [Some string] or [None] if it is not a
      valid conversion. This method also converts data of type BLOB to a string.
  *)

  val to_int : t -> int option
  (** [to_int data] converts [data] to [Some int] or [None] if it is not a valid
      conversion.

      @raise Failure if the integer conversion over- or underflows. *)

  val to_nativeint : t -> nativeint option
  (** [to_nativeint data] converts [data] to [Some nativeint] or [None] if it is
      not a valid conversion.

      @raise Failure if the integer conversion over- or underflows. *)

  val to_int32 : t -> int32 option
  (** [to_int32 data] converts [data] to [Some int32] or [None] if it is not a
      valid conversion.

      @raise Failure if the integer conversion over- or underflows. *)

  val to_int64 : t -> int64 option
  (** [to_int64 data] converts [data] to [Some int64] or [None] if it is not a
      valid conversion. *)

  val to_float : t -> float option
  (** [to_float data] converts [data] to [Some float] or [None] if it is not a
      valid conversion. *)

  val to_bool : t -> bool option
  (** [to_bool data] converts [data] to [Some bool] or [None] if it is not a
      valid conversion. *)

  val to_string_coerce : t -> string
  (** [to_string_coerce data] coerces [data] to a string, using coercion on
      ints, NULLs, floats, and other data types. *)

  val to_string_debug : t -> string
  (** [to_string_debug data] converts [data] to a string including the data
      constructor. The contents of blobs will not be printed, only its length.
      Useful for debugging. *)
end

module CArray : sig
  type t

  val of_int64_bigarray :
    (Int64.t, Bigarray.int64_elt, Bigarray.c_layout) Bigarray.Array1.t -> t
end

(** {2 General database operations} *)

val db_open :
  ?mode:[ `READONLY | `NO_CREATE ] ->
  ?uri:bool ->
  ?memory:bool ->
  ?mutex:[ `NO | `FULL ] ->
  ?cache:[ `SHARED | `PRIVATE ] ->
  ?vfs:string ->
  string ->
  db
(** [db_open ?mode ?uri ?memory ?mutex ?cache ?vfs filename] opens the database
    file [filename], and returns a database handle.

    Special filenames: ":memory:" and "" open an in-memory or temporary database
    respectively. Behaviour explained here:
    https://www.sqlite.org/inmemorydb.html

    The optional arguments [mode], [uri], [memory] and [mutex] are only
    meaningful with SQLite versions >= 3.5, [cache] only for versions >= 3.6.18.
    For older versions an exception will be raised if any of them is set to a
    non-default value. The database is opened read-only if [`READONLY] is passed
    as mode. The database file will not be created if it is missing and
    [`NO_CREATE] is set. The [uri] parameter enables URI filename interpretation
    and corresponds to [SQLITE_OPEN_URI] in the SQLite3 API. The [memory]
    parameter opens an in-memory database and corresponds to
    [SQLITE_OPEN_MEMORY] in the SQLite3 API. [mutex] determines how the database
    is accessed. The mutex parameters [`NO] and [`FULL] correspond to
    [SQLITE_OPEN_NOMUTEX] and [SQLITE_OPEN_FULLMUTEX] in the SQLite3 API
    respectively. The cache parameters [`SHARED] and [`PRIVATE] correspond to
    [SQLITE_OPEN_SHAREDCACHE] and [SQLITE_OPEN_PRIVATECACHE] in the SQLite3 API
    respectively.

    @param mode default = read-write, create
    @param uri default = false
    @param memory default = false
    @param mutex default = nothing
    @param cache default = nothing
    @param vfs default = nothing *)

val db_close : db -> bool
(** [db_close db] closes database [db] and invalidates the handle.
    @return
      [false] if database was busy (database not closed in this case!), [true]
      otherwise.

    @raise SqliteError if an invalid database handle is passed. *)

val ( let& ) : db -> (db -> 'a) -> 'a
(** [let& db = db_open "..." in ...scope that uses db...] ensures that the
    database [db] is safely closed at the end of the scope, even if there is an
    exception somewhere in the scope.

    @raise Fun.Finally_raised if the database could not be closed successfully.
*)

val enable_load_extension : db -> bool -> bool
(** [enable_load_extension db onoff] enable/disable the SQLite3 load extension.
    @return [false] if the operation fails, [true] otherwise. *)

val errcode : db -> Rc.t
(** [errcode db]
    @return the error code of the last operation on database [db].

    @raise SqliteError if an invalid database handle is passed. *)

val errmsg : db -> string
(** [errmsg db]
    @return the error message of the last operation on database [db].

    @raise SqliteError if an invalid database handle is passed. *)

val extended_errcode_int : db -> int
(** [extended_errcode_int db]
    @return
      the extended error code of the last operation on the database [db] as an
      integer.

    @raise SqliteError if an invalid database handle is passed. *)

val last_insert_rowid : db -> int64
(** [last_insert_rowid db]
    @return
      the index of the row inserted by the last operation on database [db].

    @raise SqliteError if an invalid database handle is passed. *)

val exec : db -> ?cb:(row -> headers -> unit) -> string -> Rc.t
(** [exec db ?cb sql] performs SQL-operation [sql] on database [db]. If the
    operation contains query statements, then the callback function [cb] will be
    called for each matching row. The first parameter of the callback contains
    the contents of the row, the second parameter contains the headers of the
    columns associated with the row. Exceptions raised within the callback will
    abort the execution and escape {!exec}.

    @return the return code of the operation.

    @param cb default = no callback

    @raise SqliteError if an invalid database handle is passed. *)

val exec_no_headers : db -> cb:(row -> unit) -> string -> Rc.t
(** [exec_no_headers db ?cb sql] performs SQL-operation [sql] on database [db].
    If the operation contains query statements, then the callback function [cb]
    will be called for each matching row. The parameter of the callback is the
    contents of the row. Exceptions raised within the callback will abort the
    execution and escape {!exec_no_headers}.

    @return the return code of the operation.

    @raise SqliteError if an invalid database handle is passed. *)

val exec_not_null : db -> cb:(row_not_null -> headers -> unit) -> string -> Rc.t
(** [exec_not_null db ~cb sql] performs SQL-operation [sql] on database [db]. If
    the operation contains query statements, then the callback function [cb]
    will be called for each matching row. The first parameter of the callback is
    the contents of the row, which must not contain NULL-values, the second
    paramater are the headers of the columns associated with the row. Exceptions
    raised within the callback will abort the execution and escape
    {!exec_not_null}.

    @return the return code of the operation.

    @raise SqliteError if an invalid database handle is passed.
    @raise SqliteError if a row contains NULL. *)

val exec_not_null_no_headers : db -> cb:(row_not_null -> unit) -> string -> Rc.t
(** [exec_not_null_no_headers db ~cb sql] performs SQL-operation [sql] on
    database [db]. If the operation contains query statements, then the callback
    function [cb] will be called for each matching row. The parameter of the
    callback is the contents of the row, which must not contain NULL-values.
    Exceptions raised within the callback will abort the execution and escape
    {!exec_not_null_no_headers}.

    @return the return code of the operation.

    @raise SqliteError if an invalid database handle is passed.
    @raise SqliteError if a row contains NULL. *)

val changes : db -> int
(** [changes db]
    @return
      the number of rows that were changed or inserted or deleted by the most
      recently completed SQL statement on database [db]. *)

(** {2 Prepared Statements} *)

val prepare : db -> string -> stmt
(** [prepare db sql] compile SQL-statement [sql] for database [db] into
    bytecode. The statement may be only partially compiled. In this case
    {!prepare_tail} can be called on the returned statement to compile the
    remaining part of the SQL-statement.

    NOTE: this really uses the C-function [sqlite3_prepare_v2], i.e. avoids the
    older, deprecated [sqlite3_prepare]-function.

    @raise SqliteError if an invalid database handle is passed.
    @raise SqliteError if the statement could not be prepared. *)

val prepare_or_reset : db -> stmt option ref -> string -> stmt
(** [prepare_or_reset db opt_stmt_ref sql] if [opt_stmt_ref] contains
    [Some stmt], then [stmt] will be reset and returned. Otherwise fresh
    statement [stmt] will be prepared, stored as [Some stmt] in [opt_stmt_ref]
    and then returned. This is useful for executing multiple identical commands
    in a loop, because we can more efficiently reuse the statement from previous
    iterations.

    @raise SqliteError if the statement could not be prepared or reset. *)

val prepare_tail : stmt -> stmt option
(** [prepare_tail stmt] compile the remaining part of the SQL-statement [stmt]
    to bytecode.

    NOTE: this really uses the C-function [sqlite3_prepare_v2], i.e. avoids the
    older, deprecated [sqlite3_prepare]-function.

    @return
      [None] if there was no remaining part, or [Some remaining_part] otherwise.

    @raise SqliteError if the statement could not be prepared. *)

val recompile : stmt -> unit
(** [recompile stmt] recompiles the SQL-statement associated with [stmt] to
    bytecode. The statement may be only partially compiled. In this case
    {!prepare_tail} can be called on the statement to compile the remaining part
    of the SQL-statement. Call this function if the statement expires due to
    some schema change.

    @raise SqliteError if the statement could not be recompiled. *)

val finalize : stmt -> Rc.t
(** [finalize stmt] finalizes the statement [stmt]. After finalization, the only
    valid usage of the statement is to use it in {!prepare_tail}, or to
    {!recompile} it.

    @return the return code of this operation.

    @raise SqliteError if the statement could not be finalized. *)

(** {3 Data query} *)

val data_count : stmt -> int
(** [data_count stmt]
    @return
      the number of columns in the result of the last step of statement [stmt].

    @raise SqliteError if the statement is invalid. *)

val column_count : stmt -> int
(** [column_count stmt]
    @return
      the number of columns that would be returned by executing statement
      [stmt].

    @raise SqliteError if the statement is invalid. *)

val column : stmt -> int -> Data.t
(** [column stmt n]
    @return
      the data in column [n] of the result of the last step of statement [stmt].

    @raise RangeError if [n] is out of range.
    @raise SqliteError if the statement is invalid. *)

val column_bool : stmt -> int -> bool
(** [column_bool stmt n]
    @return
      the data in column [n] of the result of the last step of statement [stmt]
      as a [bool].

    @raise RangeError if [n] is out of range.
    @raise SqliteError if the statement is invalid. *)

val column_text : stmt -> int -> string
(** [column_text stmt n]
    @return
      the data in column [n] of the result of the last step of statement [stmt]
      as text (a [string]).

    @raise RangeError if [n] is out of range.
    @raise SqliteError if the statement is invalid. *)

val column_int : stmt -> int -> int
(** [column_int stmt n]
    @return
      the data in column [n] of the result of the last step of statement [stmt]
      as an [int].

    @raise RangeError if [n] is out of range.
    @raise Failure if the integer conversion over- or underflows.
    @raise SqliteError if the statement is invalid. *)

val column_nativeint : stmt -> int -> nativeint
(** [column_nativeint stmt n]
    @return
      the data in column [n] of the result of the last step of statement [stmt]
      as a [nativeint].

    @raise RangeError if [n] is out of range.
    @raise Failure if the integer conversion over- or underflows.
    @raise SqliteError if the statement is invalid. *)

val column_int32 : stmt -> int -> int32
(** [column_int32 stmt n]
    @return
      the data in column [n] of the result of the last step of statement [stmt]
      as an [int32]. Note that this function exactly corresponds to the
      C-library function [sqlite3_column_int] and does not check for over- or
      underflow during integer conversions.

    @raise RangeError if [n] is out of range.
    @raise SqliteError if the statement is invalid. *)

val column_int64 : stmt -> int -> int64
(** [column_int64 stmt n]
    @return
      the data in column [n] of the result of the last step of statement [stmt]
      as an [int64]. Note that this function exactly corresponds to the
      C-library function [sqlite3_column_int64] and does not check for over- or
      underflow during integer conversions.

    @raise RangeError if [n] is out of range.
    @raise SqliteError if the statement is invalid. *)

val column_double : stmt -> int -> float
(** [column_double stmt n]
    @return
      the data in column [n] of the result of the last step of statement [stmt]
      as a double [float].

    @raise RangeError if [n] is out of range.
    @raise SqliteError if the statement is invalid. *)

val column_blob : stmt -> int -> string
(** [column_blob stmt n]
    @return
      the blob string in column [n] of the result of the last step of statement
      [stmt] as a [string].

    @raise RangeError if [n] is out of range.
    @raise SqliteError if the statement is invalid. *)

val column_name : stmt -> int -> header
(** [column_name stmt n]
    @return the header of column [n] in the result set of statement [stmt].

    @raise RangeError if [n] is out of range.
    @raise SqliteError if the statement is invalid. *)

val column_decltype : stmt -> int -> string option
(** [column_decltype stmt n]
    @return
      the declared type of the specified column in the result set of statement
      [stmt] as [Some str] if available, or as [None] otherwise.

    @raise RangeError if [n] is out of range.
    @raise SqliteError if the statement is invalid. *)

(** {3 Binding data to statements} *)

val bind : stmt -> int -> Data.t -> Rc.t
(** [bind stmt pos data] binds the value [data] to the free variable at position
    [pos] of statement [stmt]. NOTE: the first variable has index [1]!

    @return the return code of this operation.

    @raise RangeError if [pos] is out of range.
    @raise SqliteError if the statement is invalid. *)

val bind_text : stmt -> int -> string -> Rc.t
(** [bind_text stmt pos str] binds the string [str] to the parameter at position
    [pos] of the statement [stmt] as text.

    @return the return code of this operation.

    @raise RangeError if [pos] is out of range.
    @raise SqliteError if the statement is invalid. *)

val bind_bool : stmt -> int -> bool -> Rc.t
(** [bind_bool stmt pos b] binds the boolean [b] to the parameter at position
    [pos] of the statement [stmt] without having to manually convert it to an
    [int64] for use with [Data.INT]. [true] is turned into 1, [false] into 0.

    @return the return code of this operation.

    @raise RangeError if [pos] is out of range.
    @raise SqliteError if the statement is invalid. *)

val bind_int : stmt -> int -> int -> Rc.t
(** [bind_int stmt pos n] binds the integer [n] to the parameter at position
    [pos] of the statement [stmt] without having to manually convert it to an
    [int64] for use with [Data.INT].

    @return the return code of this operation.

    @raise RangeError if [pos] is out of range.
    @raise SqliteError if the statement is invalid. *)

val bind_nativeint : stmt -> int -> nativeint -> Rc.t
(** [bind_nativeint stmt pos n] binds the integer [n] to the parameter at
    position [pos] of the statement [stmt] without having to manually convert it
    to an [int64] for use with [Data.INT].

    @return the return code of this operation.

    @raise RangeError if [pos] is out of range.
    @raise SqliteError if the statement is invalid. *)

val bind_int32 : stmt -> int -> int32 -> Rc.t
(** [bind_int32 stmt pos n] binds the 32-bit integer [n] to the parameter at
    position [pos] of the statement [stmt] without having to manually convert it
    to an [int64] for use with [Data.INT].

    @return the return code of this operation.

    @raise RangeError if [pos] is out of range.
    @raise SqliteError if the statement is invalid. *)

val bind_int64 : stmt -> int -> int64 -> Rc.t
(** [bind_int64 stmt pos n] binds the 64-bit integer [n] to the parameter at
    position [pos] of the statement [stmt].

    @return the return code of this operation.

    @raise RangeError if [pos] is out of range.
    @raise SqliteError if the statement is invalid. *)

val bind_double : stmt -> int -> float -> Rc.t
(** [bind_double stmt pos n] binds the float [n] to the parameter at position
    [pos] of the statement [stmt].

    @return the return code of this operation.

    @raise RangeError if [pos] is out of range.
    @raise SqliteError if the statement is invalid. *)

val bind_blob : stmt -> int -> string -> Rc.t
(** [bind_blob stmt pos str] binds the string [str] to the parameter at position
    [pos] of the statement [stmt] as a blob.

    @return the return code of this operation.

    @raise RangeError if [pos] is out of range.
    @raise SqliteError if the statement is invalid. *)

val bind_carray : stmt -> int -> CArray.t -> Rc.t
(** [bind_carray stmt pos carray] binds the carray [carray] to the parameter at
    position [pos] of the statement [stmt] as a blob.

    @return the return code of this operation.

    @raise RangeError if [pos] is out of range.
    @raise SqliteError if the statement is invalid. *)

val bind_values : stmt -> Data.t list -> Rc.t
(** [bind_values stmt lst] binds the Nth element of [lst] to the Nth parameter
    of the statement.

    @return the return code of the first binding that fails, or [Rc.OK].

    @raise RangeError
      if there aren't at least as many parameters as there are elements of the
      list.
    @raise SqliteError if the statement is invalid. *)

val bind_name : stmt -> string -> Data.t -> Rc.t
(** [bind_name stmt name data] binds the value [data] to the named parameter
    [name] of statement [stmt].

    @return the return code of this operation.

    @raise Not_found if [name] does not exist.
    @raise SqliteError if the statement is invalid. *)

val bind_names : stmt -> (string * Data.t) list -> Rc.t
(** [bind_names stmt lst] binds the [(name, data)] pairs in [lst] to the
    parameters of statement [stmt].

    @return the return code of the first binding that fails, or [Rc.OK].

    @raise Not_found if a [name] does not exist.
    @raise SqliteError if the statement is invalid. *)

val bind_parameter_count : stmt -> int
(** [bind_parameter_count stmt]
    @return the number of free variables in statement [stmt].

    @raise SqliteError if the statement is invalid. *)

val bind_parameter_name : stmt -> int -> string option
(** [bind_parameter_name stmt pos]
    @return
      [Some parameter_name] of the free variable at position [pos] of statement
      [stmt], or [None] if it is ordinary ("?").

    @raise RangeError if [pos] is out of range.
    @raise SqliteError if the statement is invalid. *)

val bind_parameter_index : stmt -> string -> int
(** [bind_parameter_index stmt name]
    @return
      the position of the free variable with name [name] in statement [stmt].

    @raise Not_found if [name] was not found.
    @raise SqliteError if the statement is invalid. *)

val clear_bindings : stmt -> Rc.t
(** [clear_bindings stmt] resets all bindings associated with prepared statement
    [stmt].

    @return the return code of this operation.

    @raise SqliteError if the statement is invalid. *)

(** {3 Executing statements} *)

val step : stmt -> Rc.t
(** [step stmt] performs one step of the query associated with SQL-statement
    [stmt].

    @return the return code of this operation.

    @raise SqliteError if the step could not be executed. *)

val reset : stmt -> Rc.t
(** [reset stmt] resets the statement [stmt], e.g. to restart the query, perhaps
    with different bindings.

    @return the return code of this operation.

    @raise SqliteError if the statement could not be reset. *)

val iter : stmt -> f:(Data.t array -> unit) -> Rc.t
(** [iter stmt ~f] will call [f] once per row returned by stepping through
    [stmt]. The statement is automatically reset afterwards.

    @return [Rc.DONE] on success or another return code on error.

    @raise SqliteError if the statement is invalid. *)

val fold : stmt -> f:('a -> Data.t array -> 'a) -> init:'a -> Rc.t * 'a
(** [fold stmt ~f ~init] folds over the rows returned by [stmt] with function
    [f] and initial value [init]. The statement is automatically reset
    afterwards.

    @return
      [(rc, acc)] where [acc] is the last accumulated value returned by [f]
      after being called on a row. [rc] is [Rc.DONE] if all rows were processed
      and if the statement could be reset, otherwise an error code.

    @raise SqliteError if the statement is invalid. *)

(** {3 Stepwise query convenience functions} *)

val row_blobs : stmt -> string array
(** [row_blobs stmt]
    @return
      the blobs returned by the last query step performed with statement [stmt]
      (array of blobs).

    @raise SqliteError if the statement is invalid. *)

val row_data : stmt -> Data.t array
(** [row_data stmt]
    @return
      all data values in the row returned by the last query step performed with
      statement [stmt].

    @raise SqliteError if the statement is invalid. *)

val row_names : stmt -> headers
(** [row_names stmt]
    @return
      all column headers of the row returned by the last query step performed
      with statement [stmt].

    @raise SqliteError if the statement is invalid. *)

val row_decltypes : stmt -> string option array
(** [row_decltypes stmt]
    @return
      all column type declarations of the row returned by the last query step
      performed with statement [stmt].

    @raise SqliteError if the statement is invalid. *)

(** {2 User-defined functions} *)

val create_funN : db -> string -> (Data.t array -> Data.t) -> unit
(** [create_funN db name f] registers function [f] under name [name] with
    database handle [db]. The function has arity [N].

    @raise SqliteError if an invalid database handle is passed. *)

val create_fun0 : db -> string -> (unit -> Data.t) -> unit
(** [create_funN db name f] registers function [f] under name [name] with
    database handle [db]. The function has arity [0].

    @raise SqliteError if an invalid database handle is passed. *)

val create_fun1 : db -> string -> (Data.t -> Data.t) -> unit
(** [create_fun1 db name f] registers function [f] under name [name] with
    database handle [db]. The function has arity [1].

    @raise SqliteError if an invalid database handle is passed. *)

val create_fun2 : db -> string -> (Data.t -> Data.t -> Data.t) -> unit
(** [create_fun2 db name f] registers function [f] under name [name] with
    database handle [db]. The function has arity [2].

    @raise SqliteError if an invalid database handle is passed. *)

val create_fun3 : db -> string -> (Data.t -> Data.t -> Data.t -> Data.t) -> unit
(** [create_fun3 db name f] registers function [f] under name [name] with
    database handle [db]. The function has arity [3].

    @raise SqliteError if an invalid database handle is passed. *)

val delete_function : db -> string -> unit
(** [delete_function db name] deletes function with name [name] from database
    handle [db].

    @raise SqliteError if an invalid database handle is passed. *)

module Aggregate : sig
  (** Create user-defined aggregate and window functions.

      Aggregate functions provide the [step] function, which is called once per
      value being added to the aggregate, and the [final] function is called
      once to return the final value.

      To make a window function (requires SQLite3 3.25 or newer; on older
      versions a normal aggregate function is created), the additional [inverse]
      function, which removes a value from the window, and [value], which can be
      called many times and returns the current computed value of the window,
      must both be included. *)

  val create_fun0 :
    ?inverse:('a -> 'a) ->
    ?value:('a -> Data.t) ->
    db ->
    string ->
    init:'a ->
    step:('a -> 'a) ->
    final:('a -> Data.t) ->
    unit
  (** [create_fun0 ?inverse ?value db name ~init ~step ~final] registers the
      step and finalizer functions and optional inverse and value functions
      under name [name] with database handle [db]. This function has arity [0].

      @raise SqliteError if an invalid database handle is passed. *)

  val create_fun1 :
    ?inverse:('a -> Data.t -> 'a) ->
    ?value:('a -> Data.t) ->
    db ->
    string ->
    init:'a ->
    step:('a -> Data.t -> 'a) ->
    final:('a -> Data.t) ->
    unit
  (** [create_fun1 ?inverse ?value db name ~init ~step ~final] registers the
      step and finalizer functions and optional inverse and value functions
      under name [name] with database handle [db]. This function has arity [1].

      @raise SqliteError if an invalid database handle is passed. *)

  val create_fun2 :
    ?inverse:('a -> Data.t -> Data.t -> 'a) ->
    ?value:('a -> Data.t) ->
    db ->
    string ->
    init:'a ->
    step:('a -> Data.t -> Data.t -> 'a) ->
    final:('a -> Data.t) ->
    unit
  (** [create_fun2 ?inverse ?value db name ~init ~step ~final] registers the
      step and finalizer functions and optional inverse and value functions
      under name [name] with database handle [db]. This function has arity [2].

      @raise SqliteError if an invalid database handle is passed. *)

  val create_fun3 :
    ?inverse:('a -> Data.t -> Data.t -> Data.t -> 'a) ->
    ?value:('a -> Data.t) ->
    db ->
    string ->
    init:'a ->
    step:('a -> Data.t -> Data.t -> Data.t -> 'a) ->
    final:('a -> Data.t) ->
    unit
  (** [create_fun3 ?inverse ?value db name ~init ~step ~final] registers the
      step and finalizer functions and optional inverse and value functions
      under name [name] with database handle [db]. This function has arity [3].

      @raise SqliteError if an invalid database handle is passed. *)

  val create_funN :
    ?inverse:('a -> Data.t array -> 'a) ->
    ?value:('a -> Data.t) ->
    db ->
    string ->
    init:'a ->
    step:('a -> Data.t array -> 'a) ->
    final:('a -> Data.t) ->
    unit
  (** [create_funN ?inverse ?value db name ~init ~step ~final] registers the
      step and finalizer functions and optional inverse and value functions
      under name [name] with database handle [db]. This function has arity [N].

      @raise SqliteError if an invalid database handle is passed. *)
end

val create_collation : db -> string -> (string -> string -> int) -> unit
(** [create_collation db name func] creates a collation with [name] in database
    handle [db]. [func] is called when the collation is needed, it must return
    an integer that is negative, zero, or positive if the first string is less
    than, equal to, or greater than the second, respectively

    @raise SqliteError if an invalid database handle is passed. *)

val delete_collation : db -> string -> unit
(** [delete_collation db name] deletes collation with name [name] from database
    handle [db].

    @raise SqliteError if an invalid database handle is passed. *)

module Backup : sig
  type t
  (** Type of a backup between two databases *)

  val init : dst:db -> dst_name:string -> src:db -> src_name:string -> t
  (** [init ~dst ~dst_name ~src ~src_name] initializes a backup from the
      database [src]/[src_name] to the database [dst]/[dst_name].

      @raise SqliteError
        if there is already a read or read-write transaction open on the
        destination database *)

  val step : t -> int -> Rc.t
  (** [step backup pagecount] will copy up to [pagecount] pages between the
      associated databases of [backup]. *)

  val finish : t -> Rc.t
  (** [finish backup] destroys the association [backup]; this is to be called
      after [step] returns [SQLITE_DONE]. *)

  val remaining : t -> int
  (** [remaining backup] returns the number of pages still to be backed up in
      [backup]. *)

  val pagecount : t -> int
  (** [pagecount backup] returns the total number of pages in the source
      database of [backup]. *)
end

(** {2 Utility functions} *)

val busy_timeout : db -> int -> unit
(** [busy_timeout db ms] sets a busy handler that sleeps for a specified amount
    of time when a table is locked. The handler will sleep multiple times until
    at least [ms] milliseconds of sleeping have accumulated.

    @raise SqliteError if an invalid database handle is passed. *)

val sleep : int -> int
(** [sleep ms] sleeps at least [ms] milliseconds.
    @return
      the number of milliseconds of sleep actually requested from the operating
      system. *)
