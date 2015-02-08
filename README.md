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

### Building

SQLite3-OCaml depends upon `pkg-config` to locate and compile against an [SQLite3](http://www.sqlite.org) library compiled with [Run-Time Loadable Extensions](http://www.sqlite.org/loadext.html). 

- You can check if your library is missing the extensions by checking for the string `OMIT_LOAD_EXTENSION`. 
- If you need to redirect where `pkg-config` finds the library, set the `PKG_CONFIG_PATH` environment variable to the new directory.

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

Markus Mottl on July 10, 2012
