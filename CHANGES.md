### 5.0.0 (2019-10-11)

  * Breaking change:

      * `Data.to_string` is now `Data.to_string_coerce` to more clearly reflect
        that non-string data will be converted to strings.

  * Improved testing framework using `ppx_inline_test`.

  * Added `Sqlite3.Rc.check` for easier return code checking.

  * Added `Sqlite3.prepare_or_reset` for reusing prepared statements in loops.

  * Added more data conversion functions, also for column data for convenience.

  Thanks to Ted Spence <tspence@fb.com> for these contributions!

  * Improved closing behavior of database using new SQLite3 API.

  Thanks to Shawn <shawnw.mobile@gmail.com> for this contribution!

  * Each test case now has its own database for parallel testing.

  * Switched from `caml_alloc_custom` to `caml_alloc_custom_mem`.

  * Switched to OPAM file generation via `dune-project`


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
