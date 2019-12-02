### ?.?.? (????-??-??)

  * Added missing :with-test declaration in Dune project file.


### 5.0.0 (2019-12-01)

  * Breaking change:

      * `Data.to_string` is now `Data.to_string_coerce` to more clearly reflect
        that non-string data will be converted to strings.

  * Added support for SQLite3 window functions.

  * Added `Sqlite3.Rc.check` and `Sqlite3.Rc.is_success` for easier return
    code checking.

  * Added `Sqlite3.prepare_or_reset` for reusing prepared statements in loops.

  * Added `Sqlite3.iter` and `Sqlite3.fold` for more convenient handling of
    row data.

  * Added more data conversion functions, also for direct access to column data.

  * Added more data binding functions.

  * Improved closing behavior of database using new SQLite3 API.

  * Improved testing framework using `ppx_inline_test`.

  * Each test case now has its own database for parallel testing.

  * Switched from `caml_alloc_custom` to `caml_alloc_custom_mem`.

  * Switched to OPAM file generation via `dune-project`.

  * Improved compatibility with older OCaml versions.  Thanks to Simon Cruanes
    for this patch!

  Thanks to Shawn <shawnw.mobile@gmail.com> and Ted Spence <tspence@fb.com>
  for their work on many of these contributions!


### 4.4.1 (2018-10-25)

  * Switched to dune, dune-release, and OPAM 2.0


### 4.4.0 (2018-04-26)

  * Support for new open flags:

      * uri - for URI filename interpretation
      * memory - for in-memory databases

    Thanks to Raman Varabets for this contribution!

  * Fixed warnings and errors in configuration code due to upstream changes.

### 4.3.2 (2017-11-27)

  * Added missing -lpthread linking flag to avoid problems with projects
    that do not link with the OCaml threads library.


### 4.3.1 (2017-11-22)

  * Improved finalization of databases and statements for better performance


### 4.3.0 (2017-10-10)

  * Improved compatibility with MSVC

  * Used untagging and unboxing attributes on external functions


### 4.2.0 (2017-08-03)

  * Switched to jbuilder and topkg

  * Added backup functionality

    Thanks to Markus W. Weissmann <markus.weissmann@in.tum.de> for this
    contribution!
