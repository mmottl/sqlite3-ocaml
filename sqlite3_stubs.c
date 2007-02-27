/**************************************************************************/
/*  Copyright (c) 2005 Christian Szegedy <csdontspam871@metamatix.org>    */
/*                                                                        */
/*  Copyright (c) 2007 Jane Street Holding, LLC                           */
/*                     Author: Markus Mottl <markus.mottl@gmail.com>      */
/*                                                                        */
/*  Permission is hereby granted, free of charge, to any person           */
/*  obtaining a copy of this software and associated documentation files  */
/*  (the "Software"), to deal in the Software without restriction,        */
/*  including without limitation the rights to use, copy, modify, merge,  */
/*  publish, distribute, sublicense, and/or sell copies of the Software,  */
/*  and to permit persons to whom the Software is furnished to do so,     */
/*  subject to the following conditions:                                  */
/*                                                                        */
/*  The above copyright notice and this permission notice shall be        */
/*  included in all copies or substantial portions of the Software.       */
/*                                                                        */
/*  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,       */
/*  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES       */
/*  OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND              */
/*  NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS   */
/*  BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN    */
/*  ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN     */
/*  CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE      */
/*  SOFTWARE.                                                             */
/**************************************************************************/

#include <stdio.h>
#include <string.h>
#include <alloca.h>

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include <caml/signals.h>

#include <sqlite3.h>

/* Utility definitions */

static const value v_None = Val_int(0);


/* Type definitions */

typedef struct db_wrap {
  sqlite3 *db;
  int rc;
  int ref_count;
} db_wrap;

typedef struct stmt_wrap {
  sqlite3_stmt *stmt;
  char *sql;
  int sql_len;
  char *tail;
  db_wrap *db_wrap;
} stmt_wrap;


/* Macros to access the wrapper structures stored in the custom blocks */

#define Sqlite3_val(x) (* (db_wrap *) (Data_custom_val(x)))
#define Sqlite3_stmtw_val(x) (* (stmt_wrap *) (Data_custom_val(x)))


/* Exceptions */

static value *caml_sqlite3_InternalError = NULL;
static value *caml_sqlite3_Error = NULL;
static value *caml_sqlite3_RangeError = NULL;

static inline value raise_with_two_args(value v_tag, value v_arg1, value v_arg2)
{
  CAMLparam3(v_tag, v_arg1, v_arg2);
  value v_exc = caml_alloc_small(3, 0);
  Field(v_exc, 0) = v_tag;
  Field(v_exc, 1) = v_arg1;
  Field(v_exc, 2) = v_arg2;
  caml_raise(v_exc);
  CAMLnoreturn;
}

static inline void raise_sqlite3_InternalError(char *msg) Noreturn;

static inline void raise_sqlite3_InternalError(char *msg)
{
  raise_with_string(*caml_sqlite3_InternalError, msg);
}

static inline void range_check(int v, int max)
{
  if (v < 0 || v >= max)
    raise_with_two_args(*caml_sqlite3_RangeError, Val_int(v), Val_int(max));
}

static inline void raise_sqlite3_Error(const char *fmt, ...) Noreturn;

static inline void raise_sqlite3_Error(const char *fmt, ...)
{
  char buf[1024];
  va_list args;

  va_start(args, fmt);
  vsnprintf(buf, sizeof buf, fmt, args);
  va_end(args);

  caml_raise_with_string(*caml_sqlite3_Error, buf);
}

static inline void raise_sqlite3_misuse_db(db_wrap *dbw, const char *fmt, ...)
{
  char buf[1024];
  va_list args;

  dbw->rc = SQLITE_MISUSE;

  va_start(args, fmt);
  vsnprintf(buf, sizeof buf, fmt, args);
  va_end(args);

  raise_sqlite3_Error("%s", buf);
}

static inline void raise_sqlite3_current(sqlite3 *db, char *loc)
{
  const char *what = sqlite3_errmsg(db);
  if (!what) what = "<No error>";
  raise_sqlite3_Error("Sqlite.%s: %s", loc, what);
}

static inline void check_db(db_wrap *dbw, char *loc)
{
  if (!dbw->db)
    raise_sqlite3_misuse_db(dbw, "Sqlite3.%s called with closed database", loc);
}

static inline void raise_sqlite3_misuse_stmt(const char *fmt, ...)
{
  char buf[1024];
  va_list args;

  va_start(args, fmt);
  vsnprintf(buf, sizeof buf, fmt, args);
  va_end(args);

  caml_raise_with_string(*caml_sqlite3_Error, buf);
}

static void check_stmt(stmt_wrap *stw, char *loc)
{
  if (stw->stmt == NULL)
    raise_sqlite3_misuse_stmt("Sqlite3.%s called with finalized stmt", loc);
}

static inline stmt_wrap * safe_get_stmtw(char *loc, value v_stmt)
{
  stmt_wrap *stmtw = &Sqlite3_stmtw_val(v_stmt);
  check_stmt(stmtw, loc);
  return stmtw;
}


/* Initialisation */

CAMLprim value caml_sqlite3_init(value v_unit)
{
  caml_sqlite3_InternalError = caml_named_value("Sqlite3.InternalError");
  caml_sqlite3_Error = caml_named_value("Sqlite3.Error");
  caml_sqlite3_RangeError = caml_named_value("Sqlite3.RangeError");
  return Val_unit;
}


/* Conversion from return values */

static inline int Val_rc(int rc)
{
  if (rc >= 0) {
    if (rc <= 26) return Val_int(rc);
    if (rc >= 100 || rc <= 101) return Val_int(rc - 73);
  } else {
    value v_res = caml_alloc_small(1, 0);
    Field(v_res, 0) = Val_int(rc);
    return v_res;
  }
}


/* Copying rows */

static value copy_string_option_array(const char** strs, int len)
{
  if (!len) return Atom(0);
  else {
    CAMLparam0();
    CAMLlocal2(v_str, v_res);
    int i;

    v_res = caml_alloc(len, 0);

    for (i = 0; i < len; ++i) {
      const char *str = strs[i];
      if (str == NULL) Field(v_res, i) = v_None;
      else {
        value v_opt;
        v_str = caml_copy_string(str);
        v_opt = caml_alloc_small(1, 0);
        Field(v_opt, 0) = v_str;
        Store_field(v_res, i, v_opt);
      }
    }

    CAMLreturn(v_res);
  }
}

static value copy_not_null_string_array(const char** strs, int len)
{
  if (!len) return Atom(0);
  else {
    CAMLparam0();
    CAMLlocal1(v_res);
    int i;

    v_res = caml_alloc(len, 0);

    for(i = 0; i < len; ++i) {
      const char *str = strs[i];
      if (str == NULL) {
        v_res = (value) NULL;
        break;
      }
      else Store_field(v_res, i, caml_copy_string(str));
    }

    CAMLreturn(v_res);
  }
}

static value safe_copy_string_array(const char** strs, int len)
{
  value v_res = copy_not_null_string_array(strs, len);
  if (v_res == (value) NULL) raise_sqlite3_Error("Null element in row");
  return v_res;
}


/* Databases */

static inline void ref_count_finalize_dbw(db_wrap *dbw)
{
  if (--dbw->ref_count == 0) {
    sqlite3_close(dbw->db);
    dbw->db = NULL;
  }
}

static void dbw_finalize_gc(value v_dbw)
{
  db_wrap *dbw = &Sqlite3_val(v_dbw);
  if (dbw->db) ref_count_finalize_dbw(dbw);
}

CAMLprim value caml_sqlite3_open(value v_file)
{
  sqlite3 *db;

  if (sqlite3_open(String_val(v_file), &db)) {
    const char *msg;
    char buf[1024];
    if (db) {
      msg = sqlite3_errmsg(db);
      sqlite3_close(db);
    }
    else msg = "<unknown_error>";
    snprintf(buf, sizeof(buf) - 1, "error opening database: %s", msg);
    raise_sqlite3_Error(buf);
  } else if (db == NULL)
    raise_sqlite3_InternalError(
      "open returned neither a database nor an error");
  else {
    value v_res =
      caml_alloc_final(sizeof(db_wrap) + 1, dbw_finalize_gc, 1, 100);
    db_wrap *dbw = &Sqlite3_val(v_res);
    dbw->db = db;
    dbw->rc = SQLITE_OK;
    dbw->ref_count = 1;
    return v_res;
  }
}

CAMLprim value caml_sqlite3_close(value v_db)
{
  int ret, not_busy;
  db_wrap *dbw = &Sqlite3_val(v_db);
  check_db(dbw, "close");
  ret = sqlite3_close(dbw->db);
  not_busy = ret != SQLITE_BUSY;
  if (not_busy) dbw->db = NULL;
  return Val_bool(not_busy);
}


/* Informational functions */

CAMLprim value caml_sqlite3_errcode(value v_db)
{
  db_wrap *dbw = &Sqlite3_val(v_db);
  check_db(dbw, "errcode");
  return Val_rc(sqlite3_errcode(dbw->db));
}

CAMLprim value caml_sqlite3_errmsg(value v_db)
{
  db_wrap *dbw = &Sqlite3_val(v_db);
  value v_msg;
  check_db(dbw, "errmsg");
  v_msg = caml_copy_string(sqlite3_errmsg(dbw->db));
  return v_msg;
}

CAMLprim value caml_sqlite3_last_insert_rowid (value v_db)
{
  db_wrap *dbw = &Sqlite3_val(v_db);
  check_db(dbw, "last_insert_rowid");
  return caml_copy_int64(sqlite3_last_insert_rowid(dbw->db));
}


/* Execution and callbacks */

struct callback_with_exn {
  value *cbp;
  value exn;
};

static int exec_callback(void *cbx_, int num_columns, char **row, char **header)
{
  struct callback_with_exn *cbx = cbx_;
  value v_row, v_header, v_ret;

  caml_leave_blocking_section();

    v_row = copy_string_option_array((const char **) row, num_columns);

    Begin_roots1(v_row);
      v_header = safe_copy_string_array((const char **) header, num_columns);
    End_roots();

    v_ret = callback2_exn(*cbx->cbp, v_row, v_header);

    if (Is_exception_result(v_ret)) {
      cbx->exn = Extract_exception(v_ret);
      return 1;
    }

  caml_enter_blocking_section();

  return 0;
}

CAMLprim value caml_sqlite3_exec(value v_db, value v_maybe_cb, value v_sql)
{
  CAMLparam1(v_db);
  CAMLlocal1(v_cb);
  struct callback_with_exn cbx;
  db_wrap *dbw = &Sqlite3_val(v_db);
  size_t len;
  char *sql;
  int rc;
  sqlite3_callback cb = NULL;

  check_db(dbw, "exec");

  len = caml_string_length(v_sql);
  sql = memcpy(alloca(len + 1), String_val(v_sql), len);

  sql[len] = '\0';

  cbx.cbp = &v_cb;
  cbx.exn = Val_unit;

  if (v_maybe_cb != v_None) {
    v_cb = Field(v_maybe_cb, 0);
    cb = exec_callback;
  }

  caml_enter_blocking_section();

  rc = sqlite3_exec(dbw->db, sql, cb, (void *) &cbx, NULL);

  if (rc == SQLITE_ABORT) caml_raise(cbx.exn);
  else {
    caml_leave_blocking_section();
    CAMLreturn(Val_rc(rc));
  }
}

static int exec_callback_no_headers(
  void *cbx_, int num_columns, char **row, char **header)
{
  struct callback_with_exn *cbx = cbx_;
  value v_row, v_ret;

  caml_leave_blocking_section();

    v_row = copy_string_option_array((const char **) row, num_columns);

    v_ret = callback_exn(*cbx->cbp, v_row);

    if (Is_exception_result(v_ret)) {
      cbx->exn = Extract_exception(v_ret);
      return 1;
    }

  caml_enter_blocking_section();

  return 0;
}

CAMLprim value caml_sqlite3_exec_no_headers(value v_db, value v_cb, value v_sql)
{
  CAMLparam2(v_db, v_cb);
  struct callback_with_exn cbx;
  db_wrap *dbw = &Sqlite3_val(v_db);
  size_t len;
  char *sql;
  int rc;

  check_db(dbw, "exec");

  len = caml_string_length(v_sql);
  sql = memcpy(alloca(len + 1), String_val(v_sql), len);

  sql[len] = '\0';

  cbx.cbp = &v_cb;
  cbx.exn = Val_unit;

  caml_enter_blocking_section();

  rc =
    sqlite3_exec(dbw->db, sql, exec_callback_no_headers, (void *) &cbx, NULL);

  if (rc == SQLITE_ABORT) caml_raise(cbx.exn);
  else {
    caml_leave_blocking_section();
    CAMLreturn(Val_rc(rc));
  }
}

static int exec_not_null_callback(
  void *cbx_, int num_columns, char **row, char **header)
{
  struct callback_with_exn *cbx = cbx_;
  value v_row, v_header, v_ret;

  caml_leave_blocking_section();

    v_row = copy_not_null_string_array((const char **) row, num_columns);

    if (v_row == (value) NULL) return 1;

    Begin_roots1(v_row);
      v_header = safe_copy_string_array((const char **) header, num_columns);
    End_roots();

    v_ret = callback2_exn(*cbx->cbp, v_row, v_header);

    if (Is_exception_result(v_ret)) {
      cbx->exn = Extract_exception(v_ret);
      return 1;
    }

  caml_enter_blocking_section();

  return 0;
}

CAMLprim value caml_sqlite3_exec_not_null(
  value v_db, value v_cb, value v_sql)
{
  CAMLparam2(v_db, v_cb);
  struct callback_with_exn cbx;
  db_wrap *dbw = &Sqlite3_val(v_db);
  size_t len;
  char *sql;
  int rc;

  check_db(dbw, "exec_not_null");

  len = caml_string_length(v_sql);
  sql = memcpy(alloca(len + 1), String_val(v_sql), len);

  sql[len] = '\0';

  cbx.cbp = &v_cb;
  cbx.exn = Val_unit;

  caml_enter_blocking_section();

  rc = sqlite3_exec(dbw->db, sql, exec_not_null_callback, (void *) &cbx, NULL);

  if (rc == SQLITE_ABORT) {
    if (cbx.exn != Val_unit) caml_raise(cbx.exn);
    else raise_sqlite3_Error("Null element in row");
  } else {
    caml_leave_blocking_section();
    CAMLreturn(Val_rc(rc));
  }
}

static int exec_not_null_no_headers_callback(
  void *cbx_, int num_columns, char **row, char **header)
{
  struct callback_with_exn *cbx = cbx_;
  value v_row, v_ret;

  caml_leave_blocking_section();

    v_row = copy_not_null_string_array((const char **) row, num_columns);

    if (v_row == (value) NULL) return 1;

    v_ret = callback_exn(*cbx->cbp, v_row);

    if (Is_exception_result(v_ret)) {
      cbx->exn = Extract_exception(v_ret);
      return 1;
    }

  caml_enter_blocking_section();

  return 0;
}

CAMLprim value caml_sqlite3_exec_not_null_no_headers(
  value v_db, value v_cb, value v_sql)
{
  CAMLparam2(v_db, v_cb);
  struct callback_with_exn cbx;
  db_wrap *dbw = &Sqlite3_val(v_db);
  size_t len;
  char *sql;
  int rc;

  check_db(dbw, "exec_not_null_no_headers");

  len = caml_string_length(v_sql);
  sql = memcpy(alloca(len + 1), String_val(v_sql), len);

  sql[len] = '\0';

  cbx.cbp = &v_cb;
  cbx.exn = Val_unit;

  caml_enter_blocking_section();

  rc =
    sqlite3_exec(
      dbw->db, sql, exec_not_null_no_headers_callback, (void *) &cbx, NULL);

  if (rc == SQLITE_ABORT) {
    if (cbx.exn != Val_unit) caml_raise(cbx.exn);
    else raise_sqlite3_Error("Null element in row");
  } else {
    caml_leave_blocking_section();
    CAMLreturn(Val_rc(rc));
  }
}


/* Statements */

static inline void finalize_stmt_gc(value v_stmt)
{
  stmt_wrap *stmtw = &Sqlite3_stmtw_val(v_stmt);
  sqlite3_stmt *stmt = stmtw->stmt;
  if (stmt) {
    sqlite3_finalize(stmt);
    stmtw->stmt = NULL;
  }
  if (stmtw->sql) free(stmtw->sql);
  stmtw->sql = NULL;
  ref_count_finalize_dbw(stmtw->db_wrap);
}

CAMLprim value caml_sqlite3_stmt_finalize(value v_stmt)
{
  stmt_wrap *stmtw = safe_get_stmtw("finalize", v_stmt);
  int rc = sqlite3_finalize(stmtw->stmt);
  stmtw->stmt = NULL;
  return Val_rc(rc);
}

CAMLprim value caml_sqlite3_stmt_reset(value v_stmt)
{
  stmt_wrap *stmtw = safe_get_stmtw("reset", v_stmt);
  return Val_rc(sqlite3_reset(stmtw->stmt));
}

static value prepare_it(db_wrap *dbw, const char *sql, int sql_len)
{
  int rc;
  value v_stmt =
    caml_alloc_final(1 + sizeof(stmt_wrap), &finalize_stmt_gc, 1, 100);
  stmt_wrap *stmtw = &Sqlite3_stmtw_val(v_stmt);
  stmtw->stmt = NULL;

  if (! (stmtw->sql = malloc(sql_len + 1)))
    raise_sqlite3_Error(
      "SQL query string allocation failed for %d characters", sql_len + 1);
  else {
    stmtw->sql = memcpy(stmtw->sql, sql, sql_len);
    stmtw->sql[sql_len] = '\0';
    stmtw->sql_len = sql_len;
    stmtw->tail = NULL;
    stmtw->db_wrap = dbw;
    dbw->ref_count++;
    rc = sqlite3_prepare(dbw->db, sql, sql_len,
                         &(stmtw->stmt), (const char **) &(stmtw->tail));
    if (rc != SQLITE_OK) raise_sqlite3_current(dbw->db, "prepare");
    else if (!stmtw->stmt) raise_sqlite3_Error("No code compiled from %s", sql);
    return v_stmt;
  }
}

CAMLprim value caml_sqlite3_prepare(value v_db, value v_sql)
{
  CAMLparam1(v_db);
  db_wrap *dbw = &Sqlite3_val(v_db);
  check_db(dbw, "prepare");
  CAMLreturn(prepare_it(dbw, String_val(v_sql), string_length(v_sql)));
}

CAMLprim value caml_sqlite3_prepare_tail(value v_stmt)
{
  CAMLparam1(v_stmt);
  stmt_wrap *stmtw = &Sqlite3_stmtw_val(v_stmt);
  if (stmtw->sql && stmtw->tail && *(stmtw->tail)) {
    value v_tmp =
      prepare_it(stmtw->db_wrap, stmtw->tail,
                 stmtw->sql_len - (stmtw->tail - stmtw->sql));
    value v_res = caml_alloc_small(1, 0);
    Field(v_res, 0) = v_tmp;
    CAMLreturn(v_res);
  }
  else CAMLreturn(v_None);
}

CAMLprim value caml_sqlite3_recompile(value v_stmt)
{
  CAMLparam1(v_stmt);
  stmt_wrap *stmtw = &Sqlite3_stmtw_val(v_stmt);
  sqlite3_stmt *stmt = stmtw->stmt;
  int rc;
  if (stmt) {
    sqlite3_finalize(stmt);
    stmtw->stmt = NULL;
  }
  rc =
    sqlite3_prepare(stmtw->db_wrap->db, stmtw->sql, stmtw->sql_len,
                    &(stmtw->stmt),
                    (const char **) &(stmtw->tail));
  if (rc != SQLITE_OK) raise_sqlite3_current(stmtw->db_wrap->db, "recompile");
  else if (!stmtw->stmt)
    raise_sqlite3_Error("No code recompiled from %s", stmtw->sql);
  CAMLreturn(Val_unit);
}

CAMLprim value caml_sqlite3_bind_parameter_name(value v_stmt, value v_index)
{
  CAMLparam1(v_stmt);
  stmt_wrap *stmtw = safe_get_stmtw("bind_parameter_name", v_stmt);
  sqlite3_stmt *stmt = stmtw->stmt;
  int i = Int_val(v_index);
  const char *str;
  range_check(i - 1, sqlite3_bind_parameter_count(stmt));
  str = sqlite3_bind_parameter_name(stmt, i);
  if (str) {
    CAMLlocal1(v_tmp);
    value v_res;
    v_tmp = caml_copy_string(str);
    v_res = caml_alloc_small(1,0);
    Field(v_res, 0) = v_tmp;
    CAMLreturn(v_res);
  }
  else CAMLreturn(v_None);
}

CAMLprim value caml_sqlite3_bind_parameter_index(value v_stmt, value v_parm_name)
{
  stmt_wrap *stmtw = safe_get_stmtw("bind_parameter_index", v_stmt);
  char *parm_name = String_val(v_parm_name);
  int index = sqlite3_bind_parameter_index(stmtw->stmt, parm_name);
  if (!index) caml_raise_not_found();
  else return Val_int(index);
}

CAMLprim value caml_sqlite3_bind_parameter_count(value v_stmt)
{
  stmt_wrap *stmtw = safe_get_stmtw("bind_parameter_count", v_stmt);
  return Val_int(sqlite3_bind_parameter_count(stmtw->stmt));
}

CAMLprim value caml_sqlite3_bind(value v_stmt, value v_index, value v_data)
{
  stmt_wrap *stmtw = safe_get_stmtw("bind", v_stmt);
  sqlite3_stmt *stmt = stmtw->stmt;
  int i = Int_val(v_index);
  range_check(i - 1, sqlite3_bind_parameter_count(stmt));
  if (Is_long(v_data)) {
    switch Int_val(v_data) {
      case 1 : return Val_rc(sqlite3_bind_null(stmt, i));
      default : return Val_rc(SQLITE_ERROR);
    }
  } else {
    value field = Field(v_data, 0);
    switch (Tag_val(v_data)) {
      case 0 : return Val_rc(sqlite3_bind_int64(stmt, i, Int64_val(field)));
      case 1 : return Val_rc(sqlite3_bind_double(stmt, i, Double_val(field)));
      case 2 :
        return Val_rc(sqlite3_bind_text(stmt, i,
                                        String_val(field),
                                        string_length(field),
                                        SQLITE_TRANSIENT));
      case 3 :
        return Val_rc(sqlite3_bind_blob(stmt, i,
                                        String_val(field),
                                        string_length(field),
                                        SQLITE_TRANSIENT));
    }
  }
  return Val_rc(SQLITE_ERROR);
}

/* FIXME */

/* Sorry this gives a linking error! */
#if 0
CAMLprim value caml_sqlite3_clear_bindings(value v_stmt)
{
  sqlite3_stmt *stmt = safe_get_stmtw("clear_bindings", v_stmt)->stmt;
  return Val_rc(sqlite3_clear_bindings(stmt));
}
#endif

CAMLprim value caml_sqlite3_transfer_bindings(value v_stmt1, value v_stmt2)
{
  stmt_wrap *stmtw1 = safe_get_stmtw("transfer_bindings/1", v_stmt1);
  stmt_wrap *stmtw2 = safe_get_stmtw("transfer_bindings/2", v_stmt2);
  return Val_rc(sqlite3_transfer_bindings(stmtw1->stmt, stmtw2->stmt));
}

CAMLprim value caml_sqlite3_column_name(value v_stmt, value v_index)
{
  sqlite3_stmt *stmt = safe_get_stmtw("column_name", v_stmt)->stmt;
  int i = Int_val(v_index);
  range_check(i, sqlite3_data_count(stmt));
  return caml_copy_string(sqlite3_column_name(stmt, i));
}

CAMLprim value caml_sqlite3_column_decltype(value v_stmt, value v_index)
{
  sqlite3_stmt *stmt = safe_get_stmtw("column_decltype", v_stmt)->stmt;
  int i = Int_val(v_index);
  range_check(i, sqlite3_data_count(stmt));
  return caml_copy_string(sqlite3_column_decltype(stmt, i));
}

CAMLprim value caml_sqlite3_step(value v_stmt)
{
  CAMLparam1(v_stmt);
  sqlite3_stmt *stmt = safe_get_stmtw("step", v_stmt)->stmt;
  int rc;
  caml_enter_blocking_section();
    rc = sqlite3_step(stmt);
  caml_leave_blocking_section();
  CAMLreturn(Val_rc(rc));
}

CAMLprim value caml_sqlite3_data_count(value v_stmt)
{
  sqlite3_stmt *stmt = safe_get_stmtw("data_count", v_stmt)->stmt;
  return Val_int(sqlite3_data_count(stmt));
}

CAMLprim value caml_sqlite3_column(value v_stmt, value v_index)
{
  CAMLparam0();
  CAMLlocal1(v_tmp);
  value v_res;
  sqlite3_stmt *stmt = safe_get_stmtw("column", v_stmt)->stmt;
  int len, i = Int_val(v_index);
  range_check(i, sqlite3_data_count(stmt));
  switch (sqlite3_column_type(stmt, i)) {
    case SQLITE_INTEGER :
      v_tmp = caml_copy_int64(sqlite3_column_int64(stmt, i));
      v_res = caml_alloc_small(1, 0);
      Field(v_res, 0) = v_tmp;
      break;
    case SQLITE_FLOAT :
      v_tmp = caml_copy_double(sqlite3_column_double(stmt, i));
      v_res = caml_alloc_small(1, 1);
      Field(v_res, 0) = v_tmp;
      break;
    case SQLITE3_TEXT :
      len = sqlite3_column_bytes(stmt, i);
      v_tmp = caml_alloc_string(len);
      memcpy(String_val(v_tmp), (char *) sqlite3_column_text(stmt, i), len);
      v_res = caml_alloc_small(1, 2);
      Field(v_res, 0) = v_tmp;
      break;
    case SQLITE_BLOB :
      len = sqlite3_column_bytes(stmt, i);
      v_tmp = caml_alloc_string(len);
      memcpy(String_val(v_tmp), (char *) sqlite3_column_blob(stmt, i), len);
      v_res = caml_alloc_small(1, 3);
      Field(v_res, 0) = v_tmp;
      break;
    case SQLITE_NULL :
      v_res = Val_int(1);
      break;
    default:
      v_res = v_None;
  }
  CAMLreturn(v_res);
}

/* FIXME */

/* Sorry, this gives a linking error! */
#if 0
CAMLprim value caml_sqlite3_sleep(value v_duration)
{
   sqlite3_sleep(Int_val(v_duration));
   return Val_unit;
}
#endif

CAMLprim value caml_sqlite3_expired(value v_stmt)
{
  sqlite3_stmt *stmt = safe_get_stmtw("expired", v_stmt)->stmt;
  return sqlite3_expired(stmt) ? Val_true : Val_false;
}
