# SQLite3-OCaml - SQLite3 Bindings for OCaml

## What is SQLite3-OCaml?

SQLite3-OCaml is an [OCaml](http://www.ocaml.org) library with bindings to the
[SQLite3](http://www.sqlite.org) client API. Sqlite3 is a self-contained,
serverless, zero-configuration, transactional SQL database engine with
outstanding performance.

The design of these bindings allows for a friendly coexistence with the old
(version 2) SQLite and its OCaml wrapper `ocaml-sqlite`.

## Usage

The API documentation is in file `src/sqlite3.mli` and also here:
[online](http://mmottl.github.io/sqlite3-ocaml/api/sqlite3).

SQLite3 has its own [online documentation](http://www.sqlite.org/docs.html).

### Examples

The `test`-directory in this distribution contains simple examples for
testing features of this library. You can execute the tests by running:
`dune runtest`.

### Build issues

SQLite3-OCaml depends on `pkg-config` to locate and compile against an
[SQLite3](http://www.sqlite.org) library.

If the SQLite3 version is greater than or equal to 3.3.7, the assumption is that
it supports [Run-Time Loadable Extensions](http://www.sqlite.org/loadext.html).
If this feature has been explicitly disabled in the library, building
applications will fail with something like:

```text
Undefined symbols for architecture …:
  "_sqlite3_enable_load_extension", referenced from:
      _caml_sqlite3_enable_load_extension in libsqlite3_stubs.a(sqlite3_stubs.o)
```

- You can check if your library is missing loadable extensions by searching
  it for the string `OMIT_LOAD_EXTENSION`.

- If you need to change where `pkg-config` will look for the SQLite3
  library, set the `PKG_CONFIG_PATH` environment variable to the new
  directory. Setting the `SQLITE3_OCAML_BREWCHECK` environment variable
  automates this. This will instruct the build to check for the installation
  of a _brewed_ version of SQLite and route `pkg-config` appropriately.

- You can explicitly disable run-time loadable extensions by calling
  `configure` with the flag `--disable-loadable-extensions` or by setting
  the environment variable `SQLITE3_DISABLE_LOADABLE_EXTENSIONS` if linking
  problems persist.

- Due to frequent installation issues with loadable extensions on Mac OS X,
  the default there is to disable them. You will have to explicitly enable
  them on that platform.

## Credits

- Mikhail Fedotov wrote ocaml-sqlite for SQLite version 2. His bindings
  served as a reference for this wrapper, but SQLite3 is a complete rewrite.

- Christian Szegedy wrote the initial release for SQLite version 3.

- Markus Mottl rewrote Christian's bindings for Jane Street Holding, LLC to
  clean up some issues and to make it perform better in multi-threaded
  environments.

- Enrico Tassi contributed support for user-defined scalar functions.

- Markus W. Weissmann contributed backup functionality.

## Contact Information and Contributing

Please submit bugs reports, feature requests, contributions to the
[GitHub issue tracker](https://github.com/mmottl/sqlite3-ocaml/issues).

Up-to-date information is available at: <https://mmottl.github.io/sqlite3-ocaml>
