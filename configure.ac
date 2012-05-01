AC_INIT(sqlite-ocaml, 1.6.2, markus.mottl@gmail.com)
AC_CONFIG_SRCDIR(sqlite3.mli)

AC_CHECKING(for ocaml compiler and tools)
AC_CHECK_PROG(ocamlc,ocamlc,ocamlc)
AC_CHECK_PROG(ocamlopt,ocamlopt,ocamlopt)
AC_CHECK_PROG(ocamlfind,ocamlfind,ocamlfind)

AC_PROG_INSTALL
AC_SUBST(INSTALL)

AC_CHECKING(for C compiler)
AC_PROG_CC

AC_CHECKING(for C header files)
AC_HEADER_STDC

AC_CHECKING(libraries)

AC_ARG_WITH(
  sqlite3,
  [AC_HELP_STRING([[--with-sqlite3=PATH]],[base directory for sqlite])],
  [LIBS="$LIBS -L$withval/lib"
   CPPFLAGS="$CPPFLAGS -I$withval/include"])

AC_CHECKING(for sqlite3 library)
AC_CHECK_LIB(
  sqlite3, sqlite3_open,,
  AC_ERROR([SQLite is required.  See the --with-sqlite3 configure option.]), -ldl)

AC_CHECKING(for Sqlite header files)
AC_CHECK_HEADER(sqlite3.h,,
  AC_ERROR([SQLite is required.  See the --with-sqlite3 configure option.]))

AC_CHECK_LIB(sqlite3, sqlite3_load_extension,[HAS_ENABLE_LOAD_EXTENSION=true])

AC_CHECKING(for OCaml header files)
CAMLLOC=`$ocamlc -where`
CPPFLAGS="$CPPFLAGS -I$CAMLLOC"
AC_SUBST(HAS_ENABLE_LOAD_EXTENSION)
AC_OUTPUT(Makefile)
AC_OUTPUT(META)
