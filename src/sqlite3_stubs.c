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

#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/signals.h>
#include <caml/version.h>

#include <sqlite3.h>

#if __GNUC__ >= 3
#if !defined(__FreeBSD__) && !defined(__NetBSD__) && !defined(__DragonFly) &&  \
    !__APPLE__
#define __unused __attribute__((unused))
#endif
#else
#define __unused
#endif

#if SQLITE_VERSION_NUMBER >= 3003007 && !SQLITE3_DISABLE_LOADABLE_EXTENSIONS
#define SQLITE_HAS_ENABLE_LOAD_EXTENSION
#endif

#if SQLITE_VERSION_NUMBER >= 3003009
#define my_sqlite3_prepare sqlite3_prepare_v2
#else
#define my_sqlite3_prepare sqlite3_prepare
#endif

#if SQLITE_VERSION_NUMBER >= 3005000
#define SQLITE_HAS_OPEN_V2
#endif

#if SQLITE_VERSION_NUMBER >= 3007014
#define my_sqlite3_close sqlite3_close_v2
#else
#define my_sqlite3_close sqlite3_close
#endif

#if SQLITE_VERSION_NUMBER >= 3006000
#define SQLITE_HAS_OPEN_MUTEX_PARAMS
#endif

#if SQLITE_VERSION_NUMBER >= 3006018
#define SQLITE_HAS_OPEN_CACHE_PARAMS
#endif

#ifndef _WIN32
#include <pthread.h>
#else
#include <windows.h>
typedef DWORD pthread_key_t;

static void destroy_user_exception(void *user_exc_);

static int pthread_key_create(pthread_key_t *key, void (*destructor)(void *)) {
  CAMLassert(destructor == &destroy_user_exception);
  *key = TlsAlloc();
  if (*key == TLS_OUT_OF_INDEXES)
    return GetLastError();
  else
    return 0;
}

static inline void *pthread_getspecific(pthread_key_t key) {
  return TlsGetValue(key);
}

static int pthread_setspecific(pthread_key_t key, void *value) {
  void *old = TlsGetValue(key);
  if (old)
    destroy_user_exception(old);
  return TlsSetValue(key, value);
}
#endif

/* Utility definitions */

static inline value Val_string_option(const char *str) {
  return (str == NULL) ? Val_none : caml_alloc_some(caml_copy_string(str));
}

/* Type definitions */

typedef struct user_function {
  value v_fun;
  struct user_function *next;
} user_function;

typedef struct db_wrap {
  sqlite3 *db;
  int rc;
  int ref_count;
  user_function *user_functions;
} db_wrap;

typedef struct stmt_wrap {
  sqlite3_stmt *stmt;
  char *sql;
  int sql_len;
  char *tail;
  db_wrap *db_wrap;
} stmt_wrap;

/* Handling of exceptions in user-defined SQL-functions */

/* For propagating exceptions from user-defined SQL-functions */
static pthread_key_t user_exception_key;

typedef struct user_exception {
  value exn;
} user_exception;

static inline void create_user_exception(value v_exn) {
  user_exception *user_exn = caml_stat_alloc(sizeof(user_exception));
  user_exn->exn = v_exn;
  caml_register_global_root(&user_exn->exn);
  pthread_setspecific(user_exception_key, user_exn);
}

static inline void destroy_user_exception(void *user_exc_) {
  user_exception *user_exn = user_exc_;
  caml_remove_global_root(&user_exn->exn);
  caml_stat_free(user_exn);
}

static inline void maybe_raise_user_exception(int rc) {
  if (rc == SQLITE_ERROR) {
    user_exception *user_exn = pthread_getspecific(user_exception_key);

    if (user_exn != NULL) {
      CAMLparam0();
      CAMLlocal1(v_exn);
      v_exn = user_exn->exn;
      destroy_user_exception(user_exn);
      pthread_setspecific(user_exception_key, NULL);
      caml_raise(v_exn);
      CAMLnoreturn;
    }
  }
}

/* Macros to access the wrapper structures stored in the custom blocks */

#define Sqlite3_val(x) (*((db_wrap **)Data_custom_val(x)))
#define Sqlite3_stmtw_val(x) (*((stmt_wrap **)Data_custom_val(x)))
#define Sqlite3_backup_val(x) (*((sqlite3_backup **)Data_custom_val(x)))

/* Exceptions */

static const value *caml_sqlite3_InternalError = NULL;
static const value *caml_sqlite3_Error = NULL;
static const value *caml_sqlite3_RangeError = NULL;

static inline void raise_with_two_args(value v_tag, value v_arg1,
                                       value v_arg2) {
  CAMLparam3(v_tag, v_arg1, v_arg2);
  value v_exn = caml_alloc_small(3, 0);
  Field(v_exn, 0) = v_tag;
  Field(v_exn, 1) = v_arg1;
  Field(v_exn, 2) = v_arg2;
  caml_raise(v_exn);
  CAMLnoreturn;
}

CAMLnoreturn_start static inline void
raise_sqlite3_InternalError(char *msg) CAMLnoreturn_end;

static inline void raise_sqlite3_InternalError(char *msg) {
  caml_raise_with_string(*caml_sqlite3_InternalError, msg);
}

static inline void range_check(int v, int max) {
  if (v < 0 || v >= max)
    raise_with_two_args(*caml_sqlite3_RangeError, Val_int(v), Val_int(max));
}

CAMLnoreturn_start static void raise_sqlite3_Error(const char *fmt,
                                                   ...) CAMLnoreturn_end;

static void raise_sqlite3_Error(const char *fmt, ...) {
  char buf[1024];
  va_list args;

  va_start(args, fmt);
  vsnprintf(buf, sizeof buf, fmt, args);
  va_end(args);

  caml_raise_with_string(*caml_sqlite3_Error, buf);
}

static void raise_sqlite3_misuse_db(db_wrap *dbw, const char *fmt, ...) {
  char buf[1024];
  va_list args;

  dbw->rc = SQLITE_MISUSE;

  va_start(args, fmt);
  vsnprintf(buf, sizeof buf, fmt, args);
  va_end(args);

  raise_sqlite3_Error("%s", buf);
}

static inline void raise_sqlite3_current(sqlite3 *db, const char *loc) {
  const char *what = sqlite3_errmsg(db);
  if (!what)
    what = "<No error>";
  raise_sqlite3_Error("Sqlite3.%s: %s", loc, what);
}

static inline void check_db(db_wrap *dbw, const char *loc) {
  if (!dbw->db)
    raise_sqlite3_misuse_db(dbw, "Sqlite3.%s called with closed database", loc);
}

static void raise_sqlite3_misuse_stmt(const char *fmt, ...) {
  char buf[1024];
  va_list args;

  va_start(args, fmt);
  vsnprintf(buf, sizeof buf, fmt, args);
  va_end(args);

  caml_raise_with_string(*caml_sqlite3_Error, buf);
}

static inline void check_stmt(stmt_wrap *stw, char *loc) {
  if (stw->stmt == NULL)
    raise_sqlite3_misuse_stmt("Sqlite3.%s called with finalized stmt", loc);
}

static inline stmt_wrap *safe_get_stmtw(char *loc, value v_stmt) {
  stmt_wrap *stmtw = Sqlite3_stmtw_val(v_stmt);
  check_stmt(stmtw, loc);
  return stmtw;
}

/* Initialisation */

CAMLprim value caml_sqlite3_init(value __unused v_unit) {
  caml_sqlite3_InternalError = caml_named_value("Sqlite3.InternalError");
  caml_sqlite3_Error = caml_named_value("Sqlite3.Error");
  caml_sqlite3_RangeError = caml_named_value("Sqlite3.RangeError");
  pthread_key_create(&user_exception_key, destroy_user_exception);
  return Val_unit;
}

CAMLprim value caml_sqlite3_cleanup(value __unused v_unit) {
  pthread_setspecific(user_exception_key, NULL);
  return Val_unit;
}

/* Conversion from return values */

static inline value Val_rc(int rc) {
  value v_res;
  if (rc >= 0) {
    if (rc <= 26)
      return Val_int(rc);
    if (rc == 100 || rc == 101)
      return Val_int(rc - 73);
  }
  v_res = caml_alloc_small(1, 0);
  Field(v_res, 0) = Val_int(rc);
  return v_res;
}

/* Copying rows */

static inline value copy_string_option_array(const char **strs, int len) {
  if (!len)
    return Atom(0);
  else {
    CAMLparam0();
    CAMLlocal2(v_str, v_res);
    int i;

    v_res = caml_alloc(len, 0);

    for (i = 0; i < len; ++i) {
      const char *str = strs[i];
      if (str == NULL)
        Field(v_res, i) = Val_none;
      else
        Store_field(v_res, i, caml_alloc_some(caml_copy_string(str)));
    }

    CAMLreturn(v_res);
  }
}

static inline value copy_not_null_string_array(const char **strs, int len) {
  if (!len)
    return Atom(0);
  else {
    CAMLparam0();
    CAMLlocal1(v_res);
    int i;

    v_res = caml_alloc(len, 0);

    for (i = 0; i < len; ++i) {
      const char *str = strs[i];
      if (str == NULL) {
        v_res = (value)NULL;
        break;
      } else
        Store_field(v_res, i, caml_copy_string(str));
    }

    CAMLreturn(v_res);
  }
}

static inline value safe_copy_header_strings(const char **strs, int len) {
  value v_res = copy_not_null_string_array(strs, len);
  if (v_res == (value)NULL)
    raise_sqlite3_Error("Null element in header");
  return v_res;
}

/* Databases */

static inline void ref_count_finalize_dbw(db_wrap *dbw) {
  if (--dbw->ref_count == 0) {
    user_function *link, *next;
    for (link = dbw->user_functions; link != NULL; link = next) {
      caml_remove_generational_global_root(&link->v_fun);
      next = link->next;
      caml_stat_free(link);
    }
    dbw->user_functions = NULL;
    my_sqlite3_close(dbw->db);
    caml_stat_free(dbw);
  }
}

static inline void db_wrap_finalize_gc(value v_dbw) {
  db_wrap *dbw = Sqlite3_val(v_dbw);
  if (dbw->db)
    ref_count_finalize_dbw(dbw);
}

static struct custom_operations db_wrap_ops = {
    "sqlite3_ocaml_db_wrap",    db_wrap_finalize_gc,
    custom_compare_default,     custom_hash_default,
    custom_serialize_default,   custom_deserialize_default,
    custom_compare_ext_default, custom_fixed_length_default,
};

#ifdef SQLITE_HAS_OPEN_V2
static inline int get_open_flags(value v_mode, value v_uri, value v_memory,
                                 value v_mutex, value v_cache) {
  int flags;
  switch (Int_val(v_mode)) {
  case 0:
    flags = (SQLITE_OPEN_READWRITE | SQLITE_OPEN_CREATE);
    break;
  case 1:
    flags = SQLITE_OPEN_READWRITE;
    break;
  default:
    flags = SQLITE_OPEN_READONLY;
    break;
  }
  if (Bool_val(v_uri))
    flags |= SQLITE_OPEN_URI;
  if (Bool_val(v_memory))
    flags |= SQLITE_OPEN_MEMORY;
  switch (Int_val(v_mutex)) {
  case 0:
    break;
#ifdef SQLITE_HAS_OPEN_MUTEX_PARAMS
  case 1:
    flags |= SQLITE_OPEN_NOMUTEX;
    break;
  default:
    flags |= SQLITE_OPEN_FULLMUTEX;
    break;
#else
  default:
    caml_failwith(
        "SQLite3 version < 3.6.0 does not support mutex parameters for open");
#endif
  }
  switch (Int_val(v_cache)) {
  case 0:
    break;
#ifdef SQLITE_HAS_OPEN_CACHE_PARAMS
  case 1:
    flags |= SQLITE_OPEN_SHAREDCACHE;
    break;
  default:
    flags |= SQLITE_OPEN_PRIVATECACHE;
    break;
#else
  default:
    caml_failwith(
        "SQLite3 version < 3.6.18 does not support cache parameters for open");
#endif
  }
  return flags;
}
#endif

CAMLprim value caml_sqlite3_version(value __unused v_dummy) {
  return Val_int(sqlite3_libversion_number());
}

CAMLprim value caml_sqlite3_version_info(value __unused v_dummy) {
  return caml_copy_string(sqlite3_libversion());
}

CAMLprim value caml_sqlite3_open(value v_mode, value v_uri, value v_memory,
                                 value v_mutex, value v_cache, value v_vfs_opt,
                                 value v_file) {
  sqlite3 *db;
  int res;
#ifdef SQLITE_HAS_OPEN_V2
  int flags = get_open_flags(v_mode, v_uri, v_memory, v_mutex, v_cache);
  char *vfs;
#endif
  int file_len = caml_string_length(v_file) + 1;
  char *file;

#ifdef SQLITE_HAS_OPEN_V2
  if (Is_none(v_vfs_opt))
    vfs = NULL;
  else {
    value v_vfs = Field(v_vfs_opt, 0);
    int vfs_len = caml_string_length(v_vfs) + 1;
    vfs = caml_stat_alloc(vfs_len);
    memcpy(vfs, String_val(v_vfs), vfs_len);
  }
#else
  if (Int_val(v_mode) || Bool_val(v_uri) || Bool_val(v_memory) ||
      Int_val(v_mutex) || Int_val(v_cache))
    caml_failwith("SQLite3 version < 3.5 does not support open flags");
  if (Is_some(v_vfs_opt))
    caml_failwith("SQLite3 version < 3.5 does not support VFS modules");
#endif

  file = caml_stat_alloc(file_len);
  memcpy(file, String_val(v_file), file_len);

  caml_enter_blocking_section();
#ifdef SQLITE_HAS_OPEN_V2
  res = sqlite3_open_v2(file, &db, flags, vfs);
  caml_stat_free(vfs);
#else
  res = sqlite3_open(file, &db);
#endif
  caml_stat_free(file);
  caml_leave_blocking_section();

  if (res) {
    char msg[1024];
    if (db) {
      /* Can't use sqlite3_errmsg()'s return value after closing the
         database. */
      snprintf(msg, sizeof msg, "%s", sqlite3_errmsg(db));
      my_sqlite3_close(db);
    } else {
      strcpy(msg, "<unknown_error>");
    }
    raise_sqlite3_Error("error opening database: %s", msg);
  } else if (db == NULL)
    raise_sqlite3_InternalError(
        "open returned neither a database nor an error");
  /* "open" succeded */
  {
    size_t db_wrap_size = sizeof(db_wrap);
    db_wrap *dbw = caml_stat_alloc(db_wrap_size);
    value v_res;
#if SQLITE_DBSTATUS_CACHE_USED
    int mem, hiwtr;
    int rc = sqlite3_db_status(db, SQLITE_DBSTATUS_CACHE_USED, &mem, &hiwtr, 0);
    mem = db_wrap_size + (rc ? 8192 : mem);
    v_res = caml_alloc_custom_mem(&db_wrap_ops, sizeof(db_wrap *), mem);
#else
    v_res = caml_alloc_custom(&db_wrap_ops, sizeof(db_wrap *), 1, 1000);
#endif
    dbw->db = db;
    dbw->rc = SQLITE_OK;
    dbw->ref_count = 1;
    dbw->user_functions = NULL;
    Sqlite3_val(v_res) = dbw;
    return v_res;
  }
}

CAMLprim value caml_sqlite3_open_bc(value *argv, int __unused argn) {
  return caml_sqlite3_open(argv[0], argv[1], argv[2], argv[3], argv[4], argv[5],
                           argv[6]);
}

CAMLprim value caml_sqlite3_close(value v_db) {
  int ret, not_busy;
  db_wrap *dbw = Sqlite3_val(v_db);
  check_db(dbw, "close");
  ret = my_sqlite3_close(dbw->db);
  not_busy = ret != SQLITE_BUSY;
  if (not_busy)
    dbw->db = NULL;
  return Val_bool(not_busy);
}

#ifdef SQLITE_HAS_ENABLE_LOAD_EXTENSION
CAMLprim value caml_sqlite3_enable_load_extension(value v_db, value v_onoff) {
  int ret;
  db_wrap *dbw = Sqlite3_val(v_db);
  ret = sqlite3_enable_load_extension(dbw->db, Bool_val(v_onoff));
  return Val_bool(ret == SQLITE_OK);
}
#else
CAMLprim value caml_sqlite3_enable_load_extension(value __unused v_db,
                                                  value __unused v_onoff) {
  caml_failwith("enable_load_extension: unsupported");
}
#endif

/* Informational functions */

CAMLprim value caml_sqlite3_errcode(value v_db) {
  db_wrap *dbw = Sqlite3_val(v_db);
  check_db(dbw, "errcode");
  return Val_rc(sqlite3_errcode(dbw->db));
}

CAMLprim value caml_sqlite3_errmsg(value v_db) {
  db_wrap *dbw = Sqlite3_val(v_db);
  check_db(dbw, "errmsg");
  return caml_copy_string(sqlite3_errmsg(dbw->db));
}

CAMLprim int64_t caml_sqlite3_last_insert_rowid(value v_db) {
  db_wrap *dbw = Sqlite3_val(v_db);
  check_db(dbw, "last_insert_rowid");
  return sqlite3_last_insert_rowid(dbw->db);
}

CAMLprim value caml_sqlite3_last_insert_rowid_bc(value v_db) {
  return caml_copy_int64(caml_sqlite3_last_insert_rowid(v_db));
}

/* Execution and callbacks */

typedef struct callback_with_exn {
  value *cbp;
  value *exn;
} callback_with_exn;

static inline int exec_callback(void *cbx_, int num_columns, char **row,
                                char **header) {
  callback_with_exn *cbx = cbx_;
  value v_row, v_header, v_ret;

  caml_leave_blocking_section();

  v_row = copy_string_option_array((const char **)row, num_columns);

  Begin_roots1(v_row);
  v_header = safe_copy_header_strings((const char **)header, num_columns);
  End_roots();

  v_ret = caml_callback2_exn(*cbx->cbp, v_row, v_header);

  if (Is_exception_result(v_ret)) {
    *cbx->exn = Extract_exception(v_ret);
    caml_enter_blocking_section();
    return 1;
  }

  caml_enter_blocking_section();

  return 0;
}

CAMLprim value caml_sqlite3_exec(value v_db, value v_maybe_cb, value v_sql) {
  CAMLparam1(v_db);
  CAMLlocal2(v_cb, v_exn);
  callback_with_exn cbx;
  db_wrap *dbw = Sqlite3_val(v_db);
  int len = caml_string_length(v_sql) + 1;
  char *sql;
  int rc;
  sqlite3_callback cb = NULL;

  check_db(dbw, "exec");
  sql = caml_stat_alloc(len);
  memcpy(sql, String_val(v_sql), len);
  cbx.cbp = &v_cb;
  cbx.exn = &v_exn;

  if (Is_some(v_maybe_cb)) {
    v_cb = Field(v_maybe_cb, 0);
    cb = exec_callback;
  }

  caml_enter_blocking_section();
  rc = sqlite3_exec(dbw->db, sql, cb, (void *)&cbx, NULL);
  caml_stat_free(sql);
  caml_leave_blocking_section();

  if (rc == SQLITE_ABORT)
    caml_raise(*cbx.exn);
  maybe_raise_user_exception(rc);

  CAMLreturn(Val_rc(rc));
}

static inline int exec_callback_no_headers(void *cbx_, int num_columns,
                                           char **row, char __unused **header) {
  callback_with_exn *cbx = cbx_;
  value v_row, v_ret;

  caml_leave_blocking_section();

  v_row = copy_string_option_array((const char **)row, num_columns);
  v_ret = caml_callback_exn(*cbx->cbp, v_row);

  if (Is_exception_result(v_ret)) {
    *cbx->exn = Extract_exception(v_ret);
    caml_enter_blocking_section();
    return 1;
  }

  caml_enter_blocking_section();

  return 0;
}

CAMLprim value caml_sqlite3_exec_no_headers(value v_db, value v_cb,
                                            value v_sql) {
  CAMLparam2(v_db, v_cb);
  CAMLlocal1(v_exn);
  callback_with_exn cbx;
  db_wrap *dbw = Sqlite3_val(v_db);
  int len = caml_string_length(v_sql) + 1;
  char *sql;
  int rc;

  check_db(dbw, "exec_no_headers");
  sql = caml_stat_alloc(len);
  memcpy(sql, String_val(v_sql), len);
  cbx.cbp = &v_cb;
  cbx.exn = &v_exn;

  caml_enter_blocking_section();
  rc = sqlite3_exec(dbw->db, sql, exec_callback_no_headers, (void *)&cbx, NULL);
  caml_stat_free(sql);
  caml_leave_blocking_section();

  if (rc == SQLITE_ABORT)
    caml_raise(*cbx.exn);
  maybe_raise_user_exception(rc);

  CAMLreturn(Val_rc(rc));
}

static inline int exec_not_null_callback(void *cbx_, int num_columns,
                                         char **row, char **header) {
  callback_with_exn *cbx = cbx_;
  value v_row, v_header, v_ret;

  caml_leave_blocking_section();

  v_row = copy_not_null_string_array((const char **)row, num_columns);

  if (v_row == (value)NULL) {
    caml_enter_blocking_section();
    return 1;
  }

  Begin_roots1(v_row);
  v_header = safe_copy_header_strings((const char **)header, num_columns);
  End_roots();

  v_ret = caml_callback2_exn(*cbx->cbp, v_row, v_header);

  if (Is_exception_result(v_ret)) {
    *cbx->exn = Extract_exception(v_ret);
    caml_enter_blocking_section();
    return 1;
  }

  caml_enter_blocking_section();

  return 0;
}

CAMLprim value caml_sqlite3_exec_not_null(value v_db, value v_cb, value v_sql) {
  CAMLparam2(v_db, v_cb);
  CAMLlocal1(v_exn);
  callback_with_exn cbx;
  db_wrap *dbw = Sqlite3_val(v_db);
  int len = caml_string_length(v_sql) + 1;
  char *sql;
  int rc;

  check_db(dbw, "exec_not_null");
  sql = caml_stat_alloc(len);
  memcpy(sql, String_val(v_sql), len);
  cbx.cbp = &v_cb;
  cbx.exn = &v_exn;

  caml_enter_blocking_section();
  rc = sqlite3_exec(dbw->db, sql, exec_not_null_callback, (void *)&cbx, NULL);
  caml_stat_free(sql);
  caml_leave_blocking_section();

  if (rc == SQLITE_ABORT) {
    if (*cbx.exn != 0)
      caml_raise(*cbx.exn);
    else
      raise_sqlite3_Error("Null element in row");
  }
  maybe_raise_user_exception(rc);

  CAMLreturn(Val_rc(rc));
}

static inline int exec_not_null_no_headers_callback(void *cbx_, int num_columns,
                                                    char **row,
                                                    char __unused **header) {
  callback_with_exn *cbx = cbx_;
  value v_row, v_ret;

  caml_leave_blocking_section();

  v_row = copy_not_null_string_array((const char **)row, num_columns);
  if (v_row == (value)NULL) {
    caml_enter_blocking_section();
    return 1;
  }

  v_ret = caml_callback_exn(*cbx->cbp, v_row);

  if (Is_exception_result(v_ret)) {
    *cbx->exn = Extract_exception(v_ret);
    caml_enter_blocking_section();
    return 1;
  }

  caml_enter_blocking_section();

  return 0;
}

CAMLprim value caml_sqlite3_exec_not_null_no_headers(value v_db, value v_cb,
                                                     value v_sql) {
  CAMLparam2(v_db, v_cb);
  CAMLlocal1(v_exn);
  callback_with_exn cbx;
  db_wrap *dbw = Sqlite3_val(v_db);
  int len = caml_string_length(v_sql) + 1;
  char *sql;
  int rc;

  check_db(dbw, "exec_not_null_no_headers");
  sql = caml_stat_alloc(len);
  memcpy(sql, String_val(v_sql), len);
  cbx.cbp = &v_cb;
  cbx.exn = &v_exn;

  caml_enter_blocking_section();
  rc = sqlite3_exec(dbw->db, sql, exec_not_null_no_headers_callback,
                    (void *)&cbx, NULL);
  caml_stat_free(sql);
  caml_leave_blocking_section();

  if (rc == SQLITE_ABORT) {
    if (*cbx.exn != 0)
      caml_raise(*cbx.exn);
    else
      raise_sqlite3_Error("Null element in row");
  }
  maybe_raise_user_exception(rc);

  CAMLreturn(Val_rc(rc));
}

/* Statements */

static inline void stmt_wrap_finalize_gc(value v_stmt) {
  stmt_wrap *stmtw = Sqlite3_stmtw_val(v_stmt);
  sqlite3_stmt *stmt = stmtw->stmt;
  if (stmt)
    sqlite3_finalize(stmt);
  if (stmtw->sql)
    caml_stat_free(stmtw->sql);
  ref_count_finalize_dbw(stmtw->db_wrap);
  caml_stat_free(stmtw);
}

static struct custom_operations stmt_wrap_ops = {
    "sqlite3_ocaml_stmt_wrap",  stmt_wrap_finalize_gc,
    custom_compare_default,     custom_hash_default,
    custom_serialize_default,   custom_deserialize_default,
    custom_compare_ext_default, custom_fixed_length_default};

static inline value prepare_it(db_wrap *dbw, const char *sql, int sql_len,
                               const char *loc) {
  int rc;
  stmt_wrap *stmtw = caml_stat_alloc(sizeof(stmt_wrap));
  stmtw->db_wrap = dbw;
  dbw->ref_count++;
  stmtw->sql = caml_stat_alloc(sql_len + 1);
  memcpy(stmtw->sql, sql, sql_len);
  stmtw->sql[sql_len] = '\0';
  stmtw->sql_len = sql_len;
  rc = my_sqlite3_prepare(dbw->db, stmtw->sql, sql_len, &(stmtw->stmt),
                          (const char **)&(stmtw->tail));
  if (rc != SQLITE_OK || !stmtw->stmt) {
    caml_stat_free(stmtw->sql);
    caml_stat_free(stmtw);
    if (rc != SQLITE_OK)
      raise_sqlite3_current(dbw->db, loc);
    raise_sqlite3_Error("No code compiled from %s", sql);
  } else {
#if SQLITE_STMTSTATUS_MEMUSED
    size_t mem = sizeof(stmt_wrap) + sql_len + 1 +
                 sqlite3_stmt_status(stmtw->stmt, SQLITE_STMTSTATUS_MEMUSED, 0);
    value v_stmt =
        caml_alloc_custom_mem(&stmt_wrap_ops, sizeof(stmt_wrap *), mem);
#else
    value v_stmt =
        caml_alloc_custom(&stmt_wrap_ops, sizeof(stmt_wrap *), 1, 1000);
#endif
    Sqlite3_stmtw_val(v_stmt) = stmtw;
    return v_stmt;
  }
}

CAMLprim value caml_sqlite3_stmt_finalize(value v_stmt) {
  stmt_wrap *stmtw = safe_get_stmtw("finalize", v_stmt);
  int rc = sqlite3_finalize(stmtw->stmt);
  stmtw->stmt = NULL;
  return Val_rc(rc);
}

CAMLprim value caml_sqlite3_stmt_reset(value v_stmt) {
  sqlite3_stmt *stmt = safe_get_stmtw("reset", v_stmt)->stmt;
  return Val_rc(sqlite3_reset(stmt));
}

CAMLprim value caml_sqlite3_prepare(value v_db, value v_sql) {
  CAMLparam1(v_db);
  const char *loc = "prepare", *sql = String_val(v_sql);
  db_wrap *dbw = Sqlite3_val(v_db);
  check_db(dbw, loc);
  CAMLreturn(prepare_it(dbw, sql, caml_string_length(v_sql), loc));
}

CAMLprim value caml_sqlite3_prepare_tail(value v_stmt) {
  CAMLparam1(v_stmt);
  char *loc = "prepare_tail";
  stmt_wrap *stmtw = Sqlite3_stmtw_val(v_stmt);
  if (stmtw->sql && stmtw->tail && *(stmtw->tail)) {
    db_wrap *dbw = stmtw->db_wrap;
    int tail_len = stmtw->sql_len - (stmtw->tail - stmtw->sql);
    CAMLreturn(caml_alloc_some(prepare_it(dbw, stmtw->tail, tail_len, loc)));
  } else
    CAMLreturn(Val_none);
}

CAMLprim value caml_sqlite3_recompile(value v_stmt) {
  CAMLparam1(v_stmt);
  stmt_wrap *stmtw = Sqlite3_stmtw_val(v_stmt);
  sqlite3_stmt *stmt = stmtw->stmt;
  int rc;
  if (stmt) {
    sqlite3_finalize(stmt);
    stmtw->stmt = NULL;
  }
  rc = my_sqlite3_prepare(stmtw->db_wrap->db, stmtw->sql, stmtw->sql_len,
                          &(stmtw->stmt), (const char **)&(stmtw->tail));
  if (rc != SQLITE_OK)
    raise_sqlite3_current(stmtw->db_wrap->db, "recompile");
  else if (!stmtw->stmt)
    raise_sqlite3_Error("No code recompiled from %s", stmtw->sql);
  CAMLreturn(Val_unit);
}

/* bind_parameter_count */

CAMLprim intnat caml_sqlite3_bind_parameter_count(value v_stmt) {
  sqlite3_stmt *stmt = safe_get_stmtw("bind_parameter_count", v_stmt)->stmt;
  return sqlite3_bind_parameter_count(stmt);
}

CAMLprim value caml_sqlite3_bind_parameter_count_bc(value v_stmt) {
  return Val_int(caml_sqlite3_bind_parameter_count(v_stmt));
}

/* bind_parameter_name */

CAMLprim value caml_sqlite3_bind_parameter_name(value v_stmt, intnat pos) {
  CAMLparam1(v_stmt);
  sqlite3_stmt *stmt = safe_get_stmtw("bind_parameter_name", v_stmt)->stmt;
  range_check(pos - 1, sqlite3_bind_parameter_count(stmt));
  CAMLreturn(Val_string_option(sqlite3_bind_parameter_name(stmt, pos)));
}

CAMLprim value caml_sqlite3_bind_parameter_name_bc(value v_stmt, value v_pos) {
  return caml_sqlite3_bind_parameter_name(v_stmt, Int_val(v_pos));
}

/* bind_parameter_index */

CAMLprim intnat caml_sqlite3_bind_parameter_index(value v_stmt, value v_name) {
  sqlite3_stmt *stmt = safe_get_stmtw("bind_parameter_index", v_stmt)->stmt;
  const char *parm_name = String_val(v_name);
  int index = sqlite3_bind_parameter_index(stmt, parm_name);
  if (!index)
    caml_raise_not_found();
  return index;
}

CAMLprim value caml_sqlite3_bind_parameter_index_bc(value v_stmt,
                                                    value v_name) {
  return Val_int(caml_sqlite3_bind_parameter_index(v_stmt, v_name));
}

/* bind_blob */

CAMLprim value caml_sqlite3_bind_blob(value v_stmt, intnat pos, value v_str) {
  sqlite3_stmt *stmt = safe_get_stmtw("bind_blob", v_stmt)->stmt;
  range_check(pos - 1, sqlite3_bind_parameter_count(stmt));
  return Val_rc(sqlite3_bind_blob(stmt, pos, String_val(v_str),
                                  caml_string_length(v_str), SQLITE_TRANSIENT));
}

CAMLprim value caml_sqlite3_bind_blob_bc(value v_stmt, value v_pos,
                                         value v_str) {
  return caml_sqlite3_bind_blob(v_stmt, Int_val(v_pos), v_str);
}

/* bind_double */

CAMLprim value caml_sqlite3_bind_double(value v_stmt, intnat pos, double n) {
  sqlite3_stmt *stmt = safe_get_stmtw("bind_double", v_stmt)->stmt;
  range_check(pos - 1, sqlite3_bind_parameter_count(stmt));
  return Val_rc(sqlite3_bind_double(stmt, pos, n));
}

CAMLprim value caml_sqlite3_bind_double_bc(value v_stmt, value v_pos,
                                           value v_n) {
  return caml_sqlite3_bind_double(v_stmt, Int_val(v_pos), Double_val(v_n));
}

/* bind_int32 */

CAMLprim value caml_sqlite3_bind_int32(value v_stmt, intnat pos, int32_t n) {
  sqlite3_stmt *stmt = safe_get_stmtw("bind_int32", v_stmt)->stmt;
  range_check(pos - 1, sqlite3_bind_parameter_count(stmt));
  return Val_rc(sqlite3_bind_int(stmt, pos, n));
}

CAMLprim value caml_sqlite3_bind_int32_bc(value v_stmt, value v_pos,
                                          value v_n) {
  return caml_sqlite3_bind_int32(v_stmt, Int_val(v_pos), Int32_val(v_n));
}

/* bind_int64 */

CAMLprim value caml_sqlite3_bind_int64(value v_stmt, intnat pos, int64_t n) {
  sqlite3_stmt *stmt = safe_get_stmtw("bind_int64", v_stmt)->stmt;
  range_check(pos - 1, sqlite3_bind_parameter_count(stmt));
  return Val_rc(sqlite3_bind_int64(stmt, pos, n));
}

CAMLprim value caml_sqlite3_bind_int64_bc(value v_stmt, value v_pos,
                                          value v_n) {
  return caml_sqlite3_bind_int64(v_stmt, Int_val(v_pos), Int64_val(v_n));
}

/* bind_text */

CAMLprim value caml_sqlite3_bind_text(value v_stmt, intnat pos, value v_str) {
  sqlite3_stmt *stmt = safe_get_stmtw("bind_text", v_stmt)->stmt;
  range_check(pos - 1, sqlite3_bind_parameter_count(stmt));
  return Val_rc(sqlite3_bind_text(stmt, pos, String_val(v_str),
                                  caml_string_length(v_str), SQLITE_TRANSIENT));
}

CAMLprim value caml_sqlite3_bind_text_bc(value v_stmt, value v_pos,
                                         value v_str) {
  return caml_sqlite3_bind_text(v_stmt, Int_val(v_pos), v_str);
}

/* bind */

CAMLprim value caml_sqlite3_bind(value v_stmt, intnat pos, value v_data) {
  sqlite3_stmt *stmt = safe_get_stmtw("bind", v_stmt)->stmt;
  range_check(pos - 1, sqlite3_bind_parameter_count(stmt));
  if (Is_long(v_data)) {
    switch (Int_val(v_data)) {
    case 1:
      return Val_rc(sqlite3_bind_null(stmt, pos));
    default:
      return Val_rc(SQLITE_ERROR);
    }
  } else {
    value v_field = Field(v_data, 0);
    switch (Tag_val(v_data)) {
    case 0:
      return Val_rc(sqlite3_bind_int64(stmt, pos, Int64_val(v_field)));
    case 1:
      return Val_rc(sqlite3_bind_double(stmt, pos, Double_val(v_field)));
    case 2:
      return Val_rc(sqlite3_bind_text(stmt, pos, String_val(v_field),
                                      caml_string_length(v_field),
                                      SQLITE_TRANSIENT));
    case 3:
      return Val_rc(sqlite3_bind_blob(stmt, pos, String_val(v_field),
                                      caml_string_length(v_field),
                                      SQLITE_TRANSIENT));
    }
  }
  return Val_rc(SQLITE_ERROR);
}

CAMLprim value caml_sqlite3_bind_bc(value v_stmt, value v_pos, value v_data) {
  return caml_sqlite3_bind(v_stmt, Int_val(v_pos), v_data);
}

/* clear_bindings */

CAMLprim value caml_sqlite3_clear_bindings(value v_stmt) {
  sqlite3_stmt *stmt = safe_get_stmtw("clear_bindings", v_stmt)->stmt;
  return Val_rc(sqlite3_clear_bindings(stmt));
}

CAMLprim value caml_sqlite3_column_name(value v_stmt, intnat pos) {
  CAMLparam1(v_stmt);
  sqlite3_stmt *stmt = safe_get_stmtw("column_name", v_stmt)->stmt;
  range_check(pos, sqlite3_column_count(stmt));
  CAMLreturn(caml_copy_string(sqlite3_column_name(stmt, pos)));
}

CAMLprim value caml_sqlite3_column_name_bc(value v_stmt, value v_pos) {
  return caml_sqlite3_column_name(v_stmt, Int_val(v_pos));
}

CAMLprim value caml_sqlite3_column_decltype(value v_stmt, intnat pos) {
  CAMLparam1(v_stmt);
  sqlite3_stmt *stmt = safe_get_stmtw("column_decltype", v_stmt)->stmt;
  range_check(pos, sqlite3_column_count(stmt));
  CAMLreturn(Val_string_option(sqlite3_column_decltype(stmt, pos)));
}

CAMLprim value caml_sqlite3_column_decltype_bc(value v_stmt, value v_pos) {
  return caml_sqlite3_column_decltype(v_stmt, Int_val(v_pos));
}

CAMLprim value caml_sqlite3_step(value v_stmt) {
  CAMLparam1(v_stmt);
  sqlite3_stmt *stmt = safe_get_stmtw("step", v_stmt)->stmt;
  int rc;
  caml_enter_blocking_section();
  rc = sqlite3_step(stmt);
  caml_leave_blocking_section();
  CAMLreturn(Val_rc(rc));
}

/* data_count */

CAMLprim intnat caml_sqlite3_data_count(value v_stmt) {
  sqlite3_stmt *stmt = safe_get_stmtw("data_count", v_stmt)->stmt;
  return sqlite3_data_count(stmt);
}

CAMLprim value caml_sqlite3_data_count_bc(value v_stmt) {
  return Val_int(caml_sqlite3_data_count(v_stmt));
}

/* column_count */

CAMLprim intnat caml_sqlite3_column_count(value v_stmt) {
  sqlite3_stmt *stmt = safe_get_stmtw("column_count", v_stmt)->stmt;
  return sqlite3_column_count(stmt);
}

CAMLprim value caml_sqlite3_column_count_bc(value v_stmt) {
  return Val_int(caml_sqlite3_column_count(v_stmt));
}

/* column_blob */

CAMLprim value caml_sqlite3_column_blob(value v_stmt, intnat pos) {
  CAMLparam1(v_stmt);
  int len;
  value v_str;
  sqlite3_stmt *stmt = safe_get_stmtw("column_blob", v_stmt)->stmt;
  range_check(pos, sqlite3_column_count(stmt));
  len = sqlite3_column_bytes(stmt, pos);
  v_str = caml_alloc_initialized_string(len, sqlite3_column_blob(stmt, pos));
  CAMLreturn(v_str);
}

CAMLprim value caml_sqlite3_column_blob_bc(value v_stmt, value v_pos) {
  return caml_sqlite3_column_blob(v_stmt, Int_val(v_pos));
}

/* column_double */

CAMLprim double caml_sqlite3_column_double(value v_stmt, intnat pos) {
  sqlite3_stmt *stmt = safe_get_stmtw("column_double", v_stmt)->stmt;
  range_check(pos, sqlite3_column_count(stmt));
  return sqlite3_column_double(stmt, pos);
}

CAMLprim value caml_sqlite3_column_double_bc(value v_stmt, value v_pos) {
  return caml_copy_double(caml_sqlite3_column_double(v_stmt, Int_val(v_pos)));
}

/* column_int32 */

CAMLprim int32_t caml_sqlite3_column_int32(value v_stmt, intnat pos) {
  sqlite3_stmt *stmt = safe_get_stmtw("column_int32", v_stmt)->stmt;
  range_check(pos, sqlite3_column_count(stmt));
  return sqlite3_column_int(stmt, pos);
}

CAMLprim value caml_sqlite3_column_int32_bc(value v_stmt, value v_pos) {
  return caml_copy_int32(caml_sqlite3_column_int32(v_stmt, Int_val(v_pos)));
}

/* column_int64 */

CAMLprim int64_t caml_sqlite3_column_int64(value v_stmt, intnat pos) {
  sqlite3_stmt *stmt = safe_get_stmtw("column_int64", v_stmt)->stmt;
  range_check(pos, sqlite3_column_count(stmt));
  return sqlite3_column_int64(stmt, pos);
}

CAMLprim value caml_sqlite3_column_int64_bc(value v_stmt, value v_pos) {
  return caml_copy_int64(caml_sqlite3_column_int64(v_stmt, Int_val(v_pos)));
}

/* column_text */

CAMLprim value caml_sqlite3_column_text(value v_stmt, intnat pos) {
  CAMLparam1(v_stmt);
  int len;
  value v_str;
  sqlite3_stmt *stmt = safe_get_stmtw("column_text", v_stmt)->stmt;
  range_check(pos, sqlite3_column_count(stmt));
  len = sqlite3_column_bytes(stmt, pos);
  v_str = caml_alloc_initialized_string(len,
                                        (char *)sqlite3_column_text(stmt, pos));
  CAMLreturn(v_str);
}

CAMLprim value caml_sqlite3_column_text_bc(value v_stmt, value v_pos) {
  return caml_sqlite3_column_text(v_stmt, Int_val(v_pos));
}

/* column */

CAMLprim value caml_sqlite3_column(value v_stmt, intnat pos) {
  CAMLparam1(v_stmt);
  CAMLlocal1(v_tmp);
  value v_res;
  sqlite3_stmt *stmt = safe_get_stmtw("column", v_stmt)->stmt;
  int len;
  range_check(pos, sqlite3_column_count(stmt));
  switch (sqlite3_column_type(stmt, pos)) {
  case SQLITE_INTEGER:
    v_tmp = caml_copy_int64(sqlite3_column_int64(stmt, pos));
    v_res = caml_alloc_small(1, 0);
    Field(v_res, 0) = v_tmp;
    break;
  case SQLITE_FLOAT:
    v_tmp = caml_copy_double(sqlite3_column_double(stmt, pos));
    v_res = caml_alloc_small(1, 1);
    Field(v_res, 0) = v_tmp;
    break;
  case SQLITE3_TEXT:
    len = sqlite3_column_bytes(stmt, pos);
    v_tmp = caml_alloc_initialized_string(
        len, (char *)sqlite3_column_text(stmt, pos));
    v_res = caml_alloc_small(1, 2);
    Field(v_res, 0) = v_tmp;
    break;
  case SQLITE_BLOB:
    len = sqlite3_column_bytes(stmt, pos);
    v_tmp = caml_alloc_initialized_string(len, sqlite3_column_blob(stmt, pos));
    v_res = caml_alloc_small(1, 3);
    Field(v_res, 0) = v_tmp;
    break;
  case SQLITE_NULL:
    v_res = Val_int(1);
    break;
  default:
    v_res = Val_int(0);
  }
  CAMLreturn(v_res);
}

CAMLprim value caml_sqlite3_column_bc(value v_stmt, value v_pos) {
  return caml_sqlite3_column(v_stmt, Int_val(v_pos));
}

CAMLprim intnat caml_sqlite3_sleep(intnat duration) {
  intnat res;
  caml_enter_blocking_section();
  res = sqlite3_sleep(duration);
  caml_leave_blocking_section();
  return res;
}

CAMLprim value caml_sqlite3_sleep_bc(value v_duration) {
  return Val_int(caml_sqlite3_sleep(Int_val(v_duration)));
}

/* User-defined functions */

static inline value caml_sqlite3_wrap_values(int argc, sqlite3_value **args) {
  if (argc <= 0 || args == NULL)
    return Atom(0);
  else {
    int i, len;
    CAMLparam0();
    CAMLlocal2(v_arr, v_tmp);
    value v_res;
    v_arr = caml_alloc(argc, 0);
    for (i = 0; i < argc; ++i) {
      sqlite3_value *arg = args[i];
      switch (sqlite3_value_type(arg)) {
      case SQLITE_INTEGER:
        v_tmp = caml_copy_int64(sqlite3_value_int64(arg));
        v_res = caml_alloc_small(1, 0);
        Field(v_res, 0) = v_tmp;
        break;
      case SQLITE_FLOAT:
        v_tmp = caml_copy_double(sqlite3_value_double(arg));
        v_res = caml_alloc_small(1, 1);
        Field(v_res, 0) = v_tmp;
        break;
      case SQLITE3_TEXT:
        len = sqlite3_value_bytes(arg);
        v_tmp =
            caml_alloc_initialized_string(len, (char *)sqlite3_value_text(arg));
        v_res = caml_alloc_small(1, 2);
        Field(v_res, 0) = v_tmp;
        break;
      case SQLITE_BLOB:
        len = sqlite3_value_bytes(arg);
        v_tmp = caml_alloc_initialized_string(len, sqlite3_value_blob(arg));
        v_res = caml_alloc_small(1, 3);
        Field(v_res, 0) = v_tmp;
        break;
      case SQLITE_NULL:
        v_res = Val_int(1);
        break;
      default:
        v_res = Val_none;
      }
      Store_field(v_arr, i, v_res);
    }
    CAMLreturn(v_arr);
  }
}

static inline void exception_result(sqlite3_context *ctx, value v_res) {
  value v_exn = Extract_exception(v_res);
  create_user_exception(v_exn);
  sqlite3_result_error(ctx, "OCaml callback raised an exception", -1);
}

static inline void set_sqlite3_result(sqlite3_context *ctx, value v_res) {
  if (Is_exception_result(v_res))
    exception_result(ctx, v_res);
  else if (Is_long(v_res))
    sqlite3_result_null(ctx);
  else {
    value v = Field(v_res, 0);
    switch (Tag_val(v_res)) {
    case 0:
      sqlite3_result_int64(ctx, Int64_val(v));
      break;
    case 1:
      sqlite3_result_double(ctx, Double_val(v));
      break;
    case 2:
      sqlite3_result_text(ctx, String_val(v), caml_string_length(v),
                          SQLITE_TRANSIENT);
      break;
    case 3:
      sqlite3_result_blob(ctx, String_val(v), caml_string_length(v),
                          SQLITE_TRANSIENT);
      break;
    default:
      sqlite3_result_error(ctx, "unknown value returned by callback", -1);
    }
  }
}

static void caml_sqlite3_user_function(sqlite3_context *ctx, int argc,
                                       sqlite3_value **argv) {
  user_function *data = sqlite3_user_data(ctx);
  value v_args, v_res;
  caml_leave_blocking_section();
  v_args = caml_sqlite3_wrap_values(argc, argv);
  v_res = caml_callback_exn(Field(data->v_fun, 1), v_args);
  set_sqlite3_result(ctx, v_res);
  caml_enter_blocking_section();
}

typedef struct agg_ctx {
  int initialized;
  value v_acc;
} agg_ctx;

#define MK_USER_FUNCTION_STEP_INVERSE(NAME, GET_FUN)                           \
  static void caml_sqlite3_user_function_##NAME(                               \
      sqlite3_context *ctx, int argc, sqlite3_value **argv) {                  \
    value v_args, v_res;                                                       \
    user_function *data = sqlite3_user_data(ctx);                              \
    agg_ctx *actx = sqlite3_aggregate_context(ctx, sizeof(agg_ctx));           \
    caml_leave_blocking_section();                                             \
    if (!actx->initialized) {                                                  \
      actx->v_acc = Field(data->v_fun, 1);                                     \
      /* Not a generational global root, because it is hard to imagine         \
         that there will ever be more than at most a few instances             \
         (quite probably only one in most cases). */                           \
      caml_register_global_root(&actx->v_acc);                                 \
      actx->initialized = 1;                                                   \
    }                                                                          \
    v_args = caml_sqlite3_wrap_values(argc, argv);                             \
    v_res = caml_callback2_exn(GET_FUN, actx->v_acc, v_args);                  \
    if (Is_exception_result(v_res))                                            \
      exception_result(ctx, v_res);                                            \
    else                                                                       \
      actx->v_acc = v_res;                                                     \
    caml_enter_blocking_section();                                             \
  }

#define MK_USER_FUNCTION_VALUE_FINAL(NAME, GET_FUN, REMOVE_ROOT)               \
  static void caml_sqlite3_user_function_##NAME(sqlite3_context *ctx) {        \
    user_function *data = sqlite3_user_data(ctx);                              \
    agg_ctx *actx = sqlite3_aggregate_context(ctx, sizeof(agg_ctx));           \
    value v_res;                                                               \
    caml_leave_blocking_section();                                             \
    if (!actx->initialized) {                                                  \
      v_res = caml_callback_exn(GET_FUN, Field(data->v_fun, 1));               \
      set_sqlite3_result(ctx, v_res);                                          \
    } else {                                                                   \
      v_res = caml_callback_exn(GET_FUN, actx->v_acc);                         \
      set_sqlite3_result(ctx, v_res);                                          \
      REMOVE_ROOT;                                                             \
    }                                                                          \
    caml_enter_blocking_section();                                             \
  }

MK_USER_FUNCTION_STEP_INVERSE(step, Field(data->v_fun, 2))

#if SQLITE_VERSION_NUMBER >= 3025000
MK_USER_FUNCTION_STEP_INVERSE(inverse, Field(Field(data->v_fun, 3), 0))
MK_USER_FUNCTION_VALUE_FINAL(value, Field(Field(data->v_fun, 4), 0), )
#endif

MK_USER_FUNCTION_VALUE_FINAL(final, Field(data->v_fun, 5),
                             caml_remove_global_root(&actx->v_acc))

static inline void unregister_user_function(db_wrap *db_data, value v_name) {
  user_function *prev = NULL, *link = db_data->user_functions;
  const char *name = String_val(v_name);

  while (link != NULL) {
    if (strcmp(String_val(Field(link->v_fun, 0)), name) == 0) {
      if (prev == NULL)
        db_data->user_functions = link->next;
      else
        prev->next = link->next;
      caml_remove_generational_global_root(&link->v_fun);
      caml_stat_free(link);
      break;
    }
    prev = link;
    link = link->next;
  }
}

static inline user_function *register_user_function(db_wrap *db_data,
                                                    value v_cell) {
  /* Assume parameters are already protected */
  user_function *link = caml_stat_alloc(sizeof *link);
  link->v_fun = v_cell;
  link->next = db_data->user_functions;
  caml_register_generational_global_root(&link->v_fun);
  db_data->user_functions = link;
  return link;
}

static inline user_function *
register_scalar_user_function(db_wrap *db_data, value v_name, value v_fun) {
  /* Assume parameters are already protected */
  value v_cell = caml_alloc_small(2, 0);
  Field(v_cell, 0) = v_name;
  Field(v_cell, 1) = v_fun;
  return register_user_function(db_data, v_cell);
}

static inline user_function *
register_aggregate_user_function(db_wrap *db_data, value v_name, value v_init,
                                 value v_step, value v_inverse, value v_value,
                                 value v_final) {
  /* Assume parameters are already protected */
  value v_cell = caml_alloc_small(6, 0);
  Field(v_cell, 0) = v_name;
  Field(v_cell, 1) = v_init;
  Field(v_cell, 2) = v_step;
  Field(v_cell, 3) = v_inverse;
  Field(v_cell, 4) = v_value;
  Field(v_cell, 5) = v_final;
  return register_user_function(db_data, v_cell);
}

CAMLprim value caml_sqlite3_create_function(value v_db, value v_name,
                                            intnat n_args, value v_fun) {
  CAMLparam3(v_db, v_name, v_fun);
  user_function *param;
  int rc;
  db_wrap *dbw = Sqlite3_val(v_db);
  check_db(dbw, "create_function");
  param = register_scalar_user_function(dbw, v_name, v_fun);
  rc = sqlite3_create_function(dbw->db, String_val(v_name), n_args, SQLITE_UTF8,
                               param, caml_sqlite3_user_function, NULL, NULL);
  if (rc != SQLITE_OK) {
    unregister_user_function(dbw, v_name);
    raise_sqlite3_current(dbw->db, "create_function");
  }
  CAMLreturn(Val_unit);
}

CAMLprim value caml_sqlite3_create_function_bc(value v_db, value v_name,
                                               value v_n_args, value v_fun) {
  return caml_sqlite3_create_function(v_db, v_name, Int_val(v_n_args), v_fun);
}

CAMLprim value caml_sqlite3_create_aggregate_function(
    value v_db, value v_name, intnat n_args, value v_init, value v_stepfn,
    value v_inversefn, value v_valuefn, value v_finalfn) {
  CAMLparam5(v_db, v_name, v_init, v_stepfn, v_inversefn);
  CAMLxparam2(v_valuefn, v_finalfn);
  user_function *param;
  int rc;
  db_wrap *dbw = Sqlite3_val(v_db);
  check_db(dbw, "create_aggregate_function");
  param = register_aggregate_user_function(dbw, v_name, v_init, v_stepfn,
                                           v_inversefn, v_valuefn, v_finalfn);
#if SQLITE_VERSION_NUMBER >= 3025000
  rc = sqlite3_create_window_function(
      dbw->db, String_val(v_name), n_args, SQLITE_UTF8, param,
      caml_sqlite3_user_function_step, caml_sqlite3_user_function_final,
      Is_none(v_valuefn) ? NULL : caml_sqlite3_user_function_value,
      Is_none(v_inversefn) ? NULL : caml_sqlite3_user_function_inverse, NULL);
#else
  rc = sqlite3_create_function(dbw->db, String_val(v_name), n_args, SQLITE_UTF8,
                               param, NULL, caml_sqlite3_user_function_step,
                               caml_sqlite3_user_function_final);
#endif
  if (rc != SQLITE_OK) {
    unregister_user_function(dbw, v_name);
    raise_sqlite3_current(dbw->db, "create_aggregate_function");
  }
  CAMLreturn(Val_unit);
}

CAMLprim value caml_sqlite3_create_aggregate_function_bc(value *argv,
                                                         int __unused argn) {
  return caml_sqlite3_create_aggregate_function(
      argv[0], argv[1], Int_val(argv[2]), argv[3], argv[4], argv[5], argv[6],
      argv[7]);
}

CAMLprim value caml_sqlite3_delete_function(value v_db, value v_name) {
  int rc;
  db_wrap *dbw = Sqlite3_val(v_db);
  check_db(dbw, "delete_function");
  rc = sqlite3_create_function(dbw->db, String_val(v_name), 0, SQLITE_UTF8,
                               NULL, NULL, NULL, NULL);
  if (rc != SQLITE_OK)
    raise_sqlite3_current(dbw->db, "delete_function");
  unregister_user_function(dbw, v_name);
  return Val_unit;
}

CAMLprim value caml_sqlite3_busy_timeout(value v_db, intnat ms) {
  int rc;
  db_wrap *dbw = Sqlite3_val(v_db);
  check_db(dbw, "busy_timeout");
  rc = sqlite3_busy_timeout(dbw->db, ms);
  if (rc != SQLITE_OK)
    raise_sqlite3_current(dbw->db, "busy_timeout");
  return Val_unit;
}

CAMLprim value caml_sqlite3_busy_timeout_bc(value v_db, value v_ms) {
  return caml_sqlite3_busy_timeout(v_db, Int_val(v_ms));
}

CAMLprim intnat caml_sqlite3_changes(value v_db) {
  db_wrap *dbw = Sqlite3_val(v_db);
  check_db(dbw, "changes");
  return sqlite3_changes(dbw->db);
}

CAMLprim value caml_sqlite3_changes_bc(value v_db) {
  return Val_int(caml_sqlite3_changes(v_db));
}

/* Backup functionality */

CAMLprim value caml_sqlite3_backup_init(value v_dst, value v_dst_name,
                                        value v_src, value v_src_name) {
  CAMLparam4(v_dst, v_dst_name, v_src, v_src_name);
  CAMLlocal1(v_res);
  sqlite3_backup *res;
  int dst_len, src_len;
  char *dst_name, *src_name;

  db_wrap *dst = Sqlite3_val(v_dst);
  db_wrap *src = Sqlite3_val(v_src);

  dst_len = caml_string_length(v_dst_name) + 1;
  dst_name = caml_stat_alloc(dst_len);
  memcpy(dst_name, String_val(v_dst_name), dst_len);

  src_len = caml_string_length(v_src_name) + 1;
  src_name = caml_stat_alloc(src_len);
  memcpy(src_name, String_val(v_src_name), src_len);

  caml_enter_blocking_section();

  res = sqlite3_backup_init(dst->db, dst_name, src->db, src_name);
  caml_stat_free(dst_name);
  caml_stat_free(src_name);

  caml_leave_blocking_section();

  if (NULL == res)
    raise_sqlite3_current(dst->db, "backup_init");

  Sqlite3_backup_val(v_res) = res;

  CAMLreturn(v_res);
}

CAMLprim value caml_sqlite3_backup_step(value v_backup, intnat pagecount) {
  CAMLparam1(v_backup);
  sqlite3_backup *backup = Sqlite3_backup_val(v_backup);
  int rc;

  caml_enter_blocking_section();

  rc = sqlite3_backup_step(backup, pagecount);

  caml_leave_blocking_section();

  CAMLreturn(Val_rc(rc));
}

CAMLprim value caml_sqlite3_backup_step_bc(value v_backup, value v_pagecount) {
  return caml_sqlite3_backup_step(v_backup, Int_val(v_pagecount));
}

CAMLprim value caml_sqlite3_backup_finish(value v_backup) {
  return Val_rc(sqlite3_backup_finish(Sqlite3_backup_val(v_backup)));
}

CAMLprim intnat caml_sqlite3_backup_remaining(value v_backup) {
  return sqlite3_backup_remaining(Sqlite3_backup_val(v_backup));
}

CAMLprim value caml_sqlite3_backup_remaining_bc(value v_backup) {
  return Val_int(caml_sqlite3_backup_remaining(v_backup));
}

CAMLprim intnat caml_sqlite3_backup_pagecount(value v_backup) {
  return sqlite3_backup_pagecount(Sqlite3_backup_val(v_backup));
}

CAMLprim value caml_sqlite3_backup_pagecount_bc(value v_backup) {
  return Val_int(caml_sqlite3_backup_pagecount(v_backup));
}
