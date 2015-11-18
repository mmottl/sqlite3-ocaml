SQLite3-OCaml - SQLite3 Bindings for OCaml
==========================================

---------------------------------------------------------------------------

What is SQLite3-OCaml?
----------------------

SQLite3-OCaml is an [OCaml](http://www.ocaml.org) library with bindings to the
[SQLite3](http://www.sqlite.org) client API.  Sqlite3 is a self-contained,
serverless, zero-configuration, transactional SQL database engine with
outstanding performance for many use cases.

These bindings are written in a way that enables a friendly coexistence with
the old (version 2) SQLite and its OCaml wrapper `ocaml-sqlite`.

Usage
-----

The API in file `lib/sqlite3.mli` is fully documented, and HTML-documentation
can be built using `make doc` and installed with this distribution.  It can
also be found [online](http://mmottl.github.io/sqlite3-ocaml/api).

SQLite3 has its own [online documentation](http://www.sqlite.org/docs.html).

### Examples

The `test`-directory in this distribution contains a few simple examples
for testing various features of this library.

### Build issues

SQLite3-OCaml depends on `pkg-config` to locate and compile against an
[SQLite3](http://www.sqlite.org) library.

If the SQLite3 version is larger than or equal to 3.3.7, it is assumed that it
supports [Run-Time Loadable Extensions](http://www.sqlite.org/loadext.html).
If this feature has been explicitly disabled in the library, the build will
fail with:

```
Undefined symbols for architecture ...:
  "_sqlite3_enable_load_extension", referenced from:
      _caml_sqlite3_enable_load_extension in libsqlite3_stubs.a(sqlite3_stubs.o)
     (maybe you meant: _caml_sqlite3_enable_load_extension)
```

  * You can check if your library is missing loadable extensions by searching
    it for the string `OMIT_LOAD_EXTENSION`.

  * If you need to change where `pkg-config` will look for the SQLite3
    library, set the `PKG_CONFIG_PATH` environment variable to the new
    directory.  This can be automated by setting the `SQLITE3_OCAML_BREWCHECK`
    environment variable; this will instruct the build to see if a _brewed_
    version of SQLite is installed and route `pkg-config` appropriately.

Credits
-------

  * Enrico Tassi contributed support for user-defined scalar functions.

  * Markus Mottl rewrote Christian's bindings for Jane Street Holding, LLC to
    clean up a few things and to make it perform better in multi-threaded
    environments.

  * Christian Szegedy wrote the initial release for SQLite version 3.

  * Mikhail Fedotov wrote ocaml-sqlite for SQLite version 2.  His bindings
    served as a reference for this wrapper, but sqlite3 is written completely
    from scratch since the C interface changed significantly.

---------------------------------------------------------------------------

Contact Information and Contributing
------------------------------------

In the case of bugs, feature requests, contributions and similar, please
contact the maintainers:

  * Markus Mottl <markus.mottl@gmail.com>
  * Christian Szegedy <csdontspam@metamatix.com>

Up-to-date information should be available at:
<http://mmottl.github.io/sqlite3-ocaml>

Enjoy!

Markus Mottl on February 9, 2015
