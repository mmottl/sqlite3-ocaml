(lang dune 2.7)
(name sqlite3)

(generate_opam_files true)

(source (github mmottl/sqlite3-ocaml))
(license MIT)
(homepage "https://mmottl.github.io/sqlite3-ocaml")
(documentation "https://mmottl.github.io/sqlite3-ocaml/api")

(maintainers "Markus Mottl <markus.mottl@gmail.com>")

(authors
  "Markus Mottl <markus.mottl@gmail.com>"
  "Christian Szegedy <csdontspam@metamatix.com>"
)

(package
  (name sqlite3)
  (synopsis "SQLite3 bindings for OCaml")
  (description "\
sqlite3-ocaml is an OCaml library with bindings to the SQLite3 client API.
Sqlite3 is a self-contained, serverless, zero-configuration, transactional SQL
database engine with outstanding performance for many use cases.")
  (depends
    (ocaml (>= 4.12))
    dune-configurator
    (conf-sqlite3 :build)
    (ppx_inline_test :with-test)
  )
  (tags (clib:sqlite3 clib:pthread))
)
