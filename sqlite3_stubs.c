/**************************************************************************/
/*  Copyright (c) 2005 Christian Szegedy <csdontspam871@metamatix.org>    */
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

#include <sqlite3.h>

struct db_wrap {
   sqlite3 *db;
   int      rc;
};

typedef struct stmt_wrap {
   sqlite3_stmt        *stmt;
   sqlite3             *db;
   char                *sql;
   char                *tail;
   int                  sql_len;
} stmt_wrap;

int Val_rc(int x)
{
   if( x < 100 ) { return Val_int(x); }
   else          { return Val_int(x-73); }
}

/* Macros to access the wrapper structures stored in the custom blocks */
#define Sqlite3_val(x) (*(struct db_wrap*)(Data_custom_val(x)))
#define is_db_open(x)  (Sqlite3_val(x).db != NULL)

#define Sqlite3_stmt_val(x)  (*(stmt_wrap *)(Data_custom_val(x)))
#define is_stmt_finalized(x) (Sqlite3_stmt_val(x).stmt == NULL)
 
static void
raise_sqlite3_range_error (int v,int max)
{
   CAMLparam0();
   CAMLlocal1(info);
   info = alloc(2,0);
   Store_field(info,0,Val_int(v));
   Store_field(info,1,Val_int(max));
   raise_with_arg(*caml_named_value("sqlite3 range error"), info);  
}

static value
range_check(int v,int max)
{
   CAMLparam0();
   if( (v < 0) || ( v >= max) ) {
      raise_sqlite3_range_error(v,max);
   }
   CAMLreturn(Val_unit);
}

static void
raise_sqlite3_error(const char *fmt, ...)
{
   char buf[1024];
   va_list args;

   va_start(args, fmt);
   vsnprintf(buf, sizeof buf, fmt, args);
   va_end(args);

   raise_with_string(*caml_named_value("sqlite3 error"), buf); 
}

static void raise_sqlite3_misuse_db        (value db, const char *fmt, ...)
{
   char buf[1024];
   va_list args;

   Sqlite3_val(db).rc = SQLITE_MISUSE;

   va_start(args, fmt);
   vsnprintf(buf, sizeof buf, fmt, args);
   va_end(args);
  
   raise_sqlite3_error("%s",buf); 
}


static void
raise_sqlite3_current (sqlite3 *db,char *where)
{
   char buf[4096];
   CAMLparam0();
   CAMLlocal1(msg);
   const char *what = sqlite3_errmsg(db);
   if(!what) { what = "<No error>"; }
   raise_sqlite3_error("Sqlite.%s : %s",where,what); \
}


static value
copy_string_option_array(const char** strs, int len)
{
   CAMLparam0();
   CAMLlocal3(option, str, result);
   int i = 0;

   if (!len) {
      CAMLreturn(Atom (0));
   }

   result = alloc(len, 0);

   for ( i=0; i<len; i++ ) {
      if (strs[i] == NULL) {
         Store_field(result, i, Val_int(0));
      } 
      else {
         str               = copy_string(strs[i]);
         option            = alloc_small(1, 0);
         Field(option, 0)  = str;
         Store_field(result, i, option);
      }
   }

   CAMLreturn(result);
}

static value
safe_copy_string_array(const char** strs, int len)
{
   CAMLparam0();
   CAMLlocal2(str, result);

   int i = 0;

   if ( !len ) {
      CAMLreturn(Atom (0));
   }

   result = alloc(len, 0);
  
   for( i=0; i<len; i++ ) {
      if (strs[i] == NULL) {
         break;
      } 
      else {
         Store_field(result, i, copy_string(strs[i]));
      }
   }

   if( i<len ) {
      raise_sqlite3_error ("Null element in row.");
   }
  
   CAMLreturn(result);
}

static void
caml_sqlite3_check_db(value db, char *fun)
{        
   if (!is_db_open(db))
      raise_sqlite3_misuse_db(db,
                              "Sqlite3.%s called with closed database", fun);
}


CAMLprim value
caml_sqlite3_close(value db)
{
   CAMLparam1(db);  
   caml_sqlite3_check_db(db,"close");
   sqlite3_close(Sqlite3_val(db).db);
   Sqlite3_val(db).db = NULL;
   CAMLreturn(Val_unit);
}

static void
caml_sqlite3_finalize_gc(value db_)
{
   sqlite3 *db = Sqlite3_val(db_).db;
   if( db ) {
#ifdef DEBUG
      printf("Finalizing! %p\n",db);fflush(0);
#endif
      sqlite3_close(db);
      Sqlite3_val(db_).db = NULL;
   }
}

/* Needed for finalization of db-s */
static struct custom_operations caml_sqlite3_ops = {
   "Sqlite3 database handle",
   caml_sqlite3_finalize_gc,
   custom_compare_default,
   custom_hash_default,
   custom_serialize_default,
   custom_deserialize_default
};


CAMLprim value 
caml_sqlite3_open(value filename)
{
   CAMLparam1(filename);
   CAMLlocal1(result);
   struct db_wrap st = {NULL, SQLITE_OK};
   struct db_wrap st2 = {NULL, SQLITE_OK};

   const char *fname = String_val(filename);
   if( sqlite3_open(fname, &(st.db)) ) {
      const char *msg = st.db ? sqlite3_errmsg(st.db) : NULL;
      if(!msg) { msg = "<unknown_error>"; }
      char buf[1024];
      snprintf(buf, (sizeof buf)-1, "Can't open database: %s",msg);
      if( st.db ) {
         sqlite3_close(st.db);
      }
      raise_sqlite3_error(buf);
   }
   else if (st.db == NULL) {
      raise_sqlite3_error("Inconsistency: open returned neither "
                          "a database nor an error");
   } else {
#ifdef DEBUG
      printf("opened %p\n",st.db);fflush(0);
#endif
      result =
         alloc_custom(&caml_sqlite3_ops,1 + sizeof(struct db_wrap), 1, 10);
      Sqlite3_val(result) = st;
   };

   CAMLreturn(result);
}

CAMLprim value
caml_sqlite3_errcode(value db)
{
   CAMLparam1(db);
   caml_sqlite3_check_db(db,"errcode");
   CAMLreturn(Val_rc(sqlite3_errcode(Sqlite3_val(db).db)));
}

CAMLprim value
caml_sqlite3_errmsg(value db)
{
   CAMLparam1(db);
   CAMLlocal1(msg);
   caml_sqlite3_check_db(db,"errmsg");
   msg = copy_string(sqlite3_errmsg(Sqlite3_val(db).db));
   CAMLreturn(msg);
}

CAMLprim value
caml_sqlite3_last_insert_rowid (value db)
{
   CAMLparam1(db);
   caml_sqlite3_check_db(db, "last_insert_rowid");
   CAMLreturn(copy_int64 (sqlite3_last_insert_rowid (Sqlite3_val(db).db)));
}

struct callback_with_xcp {
   value cb;
   value xcp;
};

int exec_callback(void *cbx_,int num_columns,char **row,char **header)
{
   {
      CAMLparam0();
      CAMLlocal3(row_,header_,retval);
      struct callback_with_xcp *cbx = cbx_;
      row_    = copy_string_option_array((const char **)row,num_columns);
      header_ = safe_copy_string_array((const char **)header,num_columns);
      retval  = callback2_exn(cbx->cb,row_,header_);
      if(Is_exception_result(retval)){
         cbx->xcp = Extract_exception(retval);
         CAMLreturn(1);
      }
      else {
         CAMLreturn(0);
      }
   }
}

CAMLprim value
caml_sqlite3_exec(value db,value sql_,value cb)
{
   CAMLparam3(db,sql_,cb);
   CAMLlocal1(errmsg_v);
   caml_sqlite3_check_db(db,"exec");
   
   struct callback_with_xcp cbx;
   size_t len = string_length(sql_);
   char *sql = memcpy(alloca(len+1),String_val(sql_),len);
   sql[len]='\0';
   cbx.cb  = cb;
   cbx.xcp = Val_unit;
   int rc = sqlite3_exec(Sqlite3_val(db).db,
                         sql,
                         exec_callback,
                         (void *)&cbx,
                         NULL);

   if(cbx.xcp != Val_unit) {
      caml_raise(cbx.xcp);
   }

   const char *msg = sqlite3_errmsg(Sqlite3_val(db).db);
   if(rc && strcmp(msg,"not an error")) {
      raise_sqlite3_error("%s",msg); 
   }
   
   CAMLreturn(Val_rc(rc));
}

static void
raise_sqlite3_misuse_stmt (const char *fmt, ...)
{
   char buf[1024];
   va_list args;
   
   va_start(args, fmt);
   vsnprintf(buf, sizeof buf, fmt, args);
   va_end(args);
   
   raise_with_string(*caml_named_value("sqlite3 error"), buf);
}

static void
caml_sqlite3_check_stmt(value stmt, char *fun)
{        
   if (is_stmt_finalized(stmt)) {
      raise_sqlite3_misuse_stmt("Sqlite3.%s called with finalized stmt", fun);
   }
}

static void
caml_sqlite3_stmt_finalize_gc(value stmt_)
{
   stmt_wrap *stmt_p = &Sqlite3_stmt_val(stmt_);
   if( ! stmt_p->stmt ) {
      if( stmt_p->sql ) { free(stmt_p->sql); }
      stmt_p->sql  = NULL;
      stmt_p->db   = NULL;
      stmt_p->tail = NULL;
      sqlite3_finalize(stmt_p->stmt);
      stmt_p->stmt = NULL;
   }
}

static struct custom_operations caml_sqlite3_stmt_ops = {
   "Sqlite3 stmt descriptor",
   caml_sqlite3_stmt_finalize_gc,
   custom_compare_default,
   custom_hash_default,
   custom_serialize_default,
   custom_deserialize_default
};

CAMLprim value
caml_sqlite3_stmt_finalize(value stmt_)
{
   CAMLparam1(stmt_);
   CAMLlocal1(errmsg);
   char *zErrmsg = NULL;
   int   ret_code = SQLITE_OK;
   stmt_wrap *stmt_p = &Sqlite3_stmt_val(stmt_);
   struct sqlite3_stmt *stmt = stmt_p->stmt;
   if(!stmt) {
      raise_sqlite3_misuse_stmt("Sqlite3.finalize "
                                "called with finalized stmt");
   }
  
   ret_code = sqlite3_finalize(stmt);
   stmt_p->stmt=NULL;
   CAMLreturn(Val_rc(ret_code));
}

CAMLprim value
caml_sqlite3_stmt_reset(value stmt_)
{
   CAMLparam1(stmt_);
   sqlite3_stmt *stmt = Sqlite3_stmt_val(stmt_).stmt;
   if(!stmt) {
      raise_sqlite3_misuse_stmt("Sqlite3.reset "
                                "called with finalized stmt");
   }
   
   CAMLreturn(Val_rc(sqlite3_reset(stmt)));
}

static CAMLprim value
prepare_allocated(value stmt,
                  stmt_wrap *stmt_p)
{
   CAMLparam1(stmt);
   stmt_p->stmt    = NULL;
   stmt_p->tail    = NULL;
#if  DEBUG
   printf("sql = \"%s\"\n",stmt_p->sql); fflush(0);
#endif
   fflush(0);
   sqlite3 *db = stmt_p->db;
   if( !db ) {
      raise_sqlite3_misuse_stmt("Sqlite3.prepare_allocated "
                                "called with nonexistent db");
   }
   int rc = sqlite3_prepare(stmt_p->db,stmt_p->sql,stmt_p->sql_len,
                            &(stmt_p->stmt),
                            (const char **)&(stmt_p->tail));
#ifdef DEBUG
   printf("rc=%d [%u] db:%p %p %p %s\n",rc,stmt,
          stmt_p->db,stmt_p,stmt_p->stmt,stmt_p->sql);fflush(0);
#endif
   if( rc ) {
      if( stmt_p->stmt ) {
         caml_sqlite3_stmt_finalize_gc(stmt);
      }
      raise_sqlite3_current(stmt_p->db,"prepare");
   } else {
      if( !stmt_p->stmt ) {
         caml_sqlite3_stmt_finalize_gc(stmt);
         raise_sqlite3_error("No code is compiled from %s",stmt_p->sql);
      }

#if DEBUG
      if( stmt_p->tail ) {
         printf("tail : \"%s\"\n",stmt_p->tail); fflush(0);
      }
#endif
   }
   
   CAMLreturn(stmt);
}

static CAMLprim value
caml_sqlite3_prepare_it(sqlite3    *db,
                        const char *sql,
                        int         sql_len)
{
   CAMLparam0();
   CAMLlocal1(stmt);
   stmt = alloc_custom(&caml_sqlite3_stmt_ops,
                       1 + sizeof(stmt_wrap), 1, 10);
   stmt_wrap *stmt_p = &Sqlite3_stmt_val(stmt);

   stmt_p->db      = db;
   stmt_p->sql     = memcpy(malloc(sql_len+1),sql,sql_len);
   stmt_p->sql[sql_len]='\0';
#if DEBUG
   printf("Preparing \"%s\" %d [%u]\n",stmt_p->sql,sql_len,stmt); fflush(0);
#endif
   stmt_p->sql_len = sql_len;

   CAMLreturn (prepare_allocated(stmt,stmt_p));
}

CAMLprim value
caml_sqlite3_recompile(value stmt_)
{
   CAMLparam1(stmt_);
   stmt_wrap    *stmt_p = &Sqlite3_stmt_val(stmt_);
   sqlite3_stmt *stmt = stmt_p->stmt;
   if(!stmt) {
      raise_sqlite3_misuse_stmt("Sqlite3.recompile "
                                "called with finalized stmt");
   }
   const char *sql = Sqlite3_stmt_val(stmt_).sql;
   if( !sql ) {
      raise_sqlite3_misuse_stmt("Sqlite3.recompile "
                                "called with empty string");
   }

   caml_sqlite3_stmt_finalize(stmt_);
   prepare_allocated(stmt_,stmt_p);
   CAMLreturn(Val_unit);
}
      
CAMLprim value
caml_sqlite3_bind_parameter_name(value stmt_,value index)
{
   CAMLparam1(stmt_);
   CAMLlocal2(result,tmp);
   stmt_wrap    *stmt_p = &Sqlite3_stmt_val(stmt_);
   sqlite3_stmt *stmt = stmt_p->stmt;
   if(!stmt) {
      raise_sqlite3_misuse_stmt("Sqlite3.bind_parameter_name "
                                "called with finalized stmt");
   }
   int i = Int_val(index);
   range_check(i-1,sqlite3_bind_parameter_count(stmt));

   const char *str = sqlite3_bind_parameter_name(stmt,i);
   if( str ) {
      result          = alloc_small(1,0);
      tmp             = copy_string(str); /* tmp is needed to make GC happy! */
      Field(result,0) = tmp;
      CAMLreturn(result);
   } else {
      CAMLreturn(Val_int(0));
   }
}

CAMLprim value
caml_sqlite3_bind_parameter_index(value stmt_,value parmname)
{
   CAMLparam2(stmt_,parmname);
   sqlite3_stmt *stmt = Sqlite3_stmt_val(stmt_).stmt;
   if(!stmt) {
      raise_sqlite3_misuse_stmt("Sqlite3.bind_parameter_index "
                                "called with finalized stmt");
   }
   
   CAMLreturn(Val_int(sqlite3_bind_parameter_index(stmt,
                                                   String_val(parmname))));
}

CAMLprim value
caml_sqlite3_bind_parameter_count(value stmt_)
{
   CAMLparam1(stmt_);
   sqlite3_stmt *stmt = Sqlite3_stmt_val(stmt_).stmt;
   if(!stmt) {
      raise_sqlite3_misuse_stmt("Sqlite3.bind_paramater_count "
                                "called with finalized stmt");
   }
   CAMLreturn(Val_int(sqlite3_bind_parameter_count(stmt)));
}

CAMLprim value
caml_sqlite3_bind(value stmt_,value index,value data)
{
   CAMLparam2(stmt_,data);
   CAMLlocal1(field);
   sqlite3_stmt *stmt = Sqlite3_stmt_val(stmt_).stmt;
   if(!stmt) {
      raise_sqlite3_misuse_stmt("Sqlite3.bind "
                                "called with finalized stmt");
   }
   int i = Int_val(index);
   range_check(i-1,sqlite3_bind_parameter_count(stmt));
   if(Is_long(data)) {
      switch(Int_val(data)) {
      case 1 : CAMLreturn(Val_rc(sqlite3_bind_null(stmt,i)));
      default:
         CAMLreturn(Val_rc(SQLITE_ERROR));
      }
   } else {
      field = Field(data,0);
      switch(Tag_val(data)) {
      case 0: CAMLreturn(Val_rc(sqlite3_bind_int64(stmt,i,
                                                   Int64_val(field))));
      case 1: CAMLreturn(Val_rc(sqlite3_bind_double(stmt,i,
                                                    Double_val(field))));
      case 2: CAMLreturn(Val_rc(sqlite3_bind_text(stmt,i,
                                                  String_val(field),
                                                  string_length(field),
                                                  SQLITE_TRANSIENT
                                                  )));
      case 3: CAMLreturn(Val_rc(sqlite3_bind_blob(stmt,i,
                                                  String_val(field),
                                                  string_length(field),
                                                  SQLITE_TRANSIENT
                                                  )));
      }
   }
   CAMLreturn(Val_rc(SQLITE_ERROR));
}

/* Sorry this gives a linking error! */
#if 0
CAMLprim value
caml_sqlite3_clear_bindings(value stmt_)
{
   CAMLparam1(stmt_);
   sqlite3_stmt *stmt = Sqlite3_stmt_val(stmt_).stmt;
   if(!stmt) {
      raise_sqlite3_misuse_stmt("Sqlite3.clear_bindings "
                                "called with finalized stmt");
   } 
   CAMLreturn(Val_rc(sqlite3_clear_bindings(stmt)));
}
#endif

CAMLprim value
caml_sqlite3_transfer_bindings(value stmt1_,value stmt2_)
{
   CAMLparam2(stmt1_,stmt2_);
   sqlite3_stmt *stmt1 = Sqlite3_stmt_val(stmt1_).stmt;

   if(!stmt1) {
      raise_sqlite3_misuse_stmt("Sqlite3.transfer_bindings "
                                "called with finalized source stmt");
   } 

   sqlite3_stmt *stmt2 = Sqlite3_stmt_val(stmt2_).stmt;

   if(!stmt2) {
      raise_sqlite3_misuse_stmt("Sqlite3.transfer_bindings "
                                "called with finalized target stmt");
   } 

   CAMLreturn(Val_rc(sqlite3_transfer_bindings(stmt1,stmt2)));
}


CAMLprim value
caml_sqlite3_prepare(value db, value sql_)
{
   CAMLparam2(db,sql_);
   CAMLlocal1(result);
   int len         = string_length(sql_);
   const char *sql = memcpy(alloca(len),String_val(sql_),len);
   result = caml_sqlite3_prepare_it(Sqlite3_val(db).db,sql,len);

   CAMLreturn(result);
}

CAMLprim value
caml_sqlite3_prepare_tail(value stmt_)
{
   CAMLparam1(stmt_);
   CAMLlocal2(result,tmp);
   stmt_wrap    *stmt_p = &Sqlite3_stmt_val(stmt_);
   sqlite3_stmt *stmt = stmt_p->stmt;
#ifdef DEBUG
   printf("prepare_tail :db:%p stmt_p: %p stmt: %p\n",stmt_p->db,stmt_p,stmt);fflush(0);
#endif
   if(!stmt) {
      raise_sqlite3_misuse_stmt("Sqlite3.prepare_tail "
                                "called with finalized stmt");
   }

   if(stmt_p->db && stmt_p->tail && *(stmt_p->tail) && stmt_p->sql) {
      tmp = caml_sqlite3_prepare_it(stmt_p->db,
                                    stmt_p->tail,
                                    stmt_p->sql_len -
                                    (stmt_p->tail - stmt_p->sql));
      result = alloc_small(1,0);
      Field(result,0)=tmp;
   } else {
      result = Val_int(0);
   }
   
   CAMLreturn(result);
}

CAMLprim value
caml_sqlite3_column_name(value stmt_, value index)
{
   CAMLparam1(stmt_);
   sqlite3_stmt *stmt = Sqlite3_stmt_val(stmt_).stmt;
   if(!stmt) {
      raise_sqlite3_misuse_stmt("Sqlite3.column_name "
                                "called with finalized stmt");
   }

   int i = Int_val(index);
   range_check(i,sqlite3_data_count(stmt));

   CAMLreturn(copy_string(sqlite3_column_name(stmt,i)));
}

CAMLprim value
caml_sqlite3_column_decltype(value stmt_, value index)
{
   CAMLparam1(stmt_);
   sqlite3_stmt *stmt = Sqlite3_stmt_val(stmt_).stmt;
   if(!stmt) {
      raise_sqlite3_misuse_stmt("Sqlite3.column_decltype "
                                "called with finalized stmt");
   }

   int i = Int_val(index);
   range_check(i,sqlite3_data_count(stmt));

   CAMLreturn(copy_string(sqlite3_column_decltype(stmt,i)));
}

CAMLprim value
caml_sqlite3_step(value stmt_)
{
   CAMLparam1(stmt_);
   sqlite3_stmt *stmt = Sqlite3_stmt_val(stmt_).stmt;
   if( !stmt ) {
      raise_sqlite3_misuse_stmt("Sqlite3.step "
                                "called with finalized stmt");
   }

   CAMLreturn(Val_rc(sqlite3_step(stmt)));
}

CAMLprim value
caml_sqlite3_data_count(value stmt_)
{
   CAMLparam1(stmt_);
   sqlite3_stmt *stmt = Sqlite3_stmt_val(stmt_).stmt;
   if( !stmt ) {
      raise_sqlite3_misuse_stmt("Sqlite3.data_count "
                                "called with finalized stmt");
   }

   CAMLreturn(Val_int(sqlite3_data_count(stmt)));
}

CAMLprim value
caml_sqlite3_column(value stmt_,value index)
{
   CAMLparam1(stmt_);
   CAMLlocal2(result,tmp);
   sqlite3_stmt *stmt = Sqlite3_stmt_val(stmt_).stmt;
   if( !stmt ) {
      raise_sqlite3_misuse_stmt("Sqlite3.column "
                                "called with finalized stmt");
   }
   int i = Int_val(index);
   range_check(i,sqlite3_data_count(stmt));
   int len;
   
   switch(sqlite3_column_type(stmt,i)) {
      /* WARNING : we need the tmp variable to make GC happy! */
   case SQLITE_INTEGER:
      result = alloc_small(1,0);
      tmp    = copy_int64(sqlite3_column_int64(stmt,i));
      Field(result,0)=tmp;
      break;
   case SQLITE_FLOAT:
      result = alloc_small(1,1);
      tmp    = copy_double(sqlite3_column_double(stmt,i));
      Field(result,0)=tmp;
      break;
   case SQLITE3_TEXT:
      len    = sqlite3_column_bytes(stmt,i);
      tmp    = alloc_string(len);
      memcpy(String_val(tmp),(char *)sqlite3_column_text(stmt,i),len);
      result = alloc_small(1,2);
      Field(result,0)=tmp;
      break;
   case SQLITE_BLOB:
      len    = sqlite3_column_bytes(stmt,i);
      tmp    = alloc_string(len);
      memcpy(String_val(tmp),(char *)sqlite3_column_blob(stmt,i),len);
      result = alloc_small(1,3);
      Field(result,0)=tmp;
      break;
   case SQLITE_NULL:
      result = Val_int(1);
      break;
   default:
      result = Val_int(0);
   }

   CAMLreturn(result);
}

/* Sorry, this gives a linking error! */
CAMLprim value
caml_sqlite3_sleep(value duration)
{
   /* sqlite3_sleep(Int_val(duration)); */
   return Val_unit;
}

CAMLprim value
caml_sqlite3_expired(value stmt_)
{
   CAMLparam1(stmt_);
   sqlite3_stmt *stmt = Sqlite3_stmt_val(stmt_).stmt;
   if( !stmt ) {
      raise_sqlite3_misuse_stmt("Sqlite3.column "
                                "called with finalized stmt");
   }
   CAMLreturn(sqlite3_expired(stmt)?Val_true:Val_false);
}
