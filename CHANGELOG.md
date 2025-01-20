# Changelog

## [5.3.0] - 2025-01-20

### Added

- Collation support. Thanks to Gusted.

### Changed

- Builds now respect `$PKG_CONFIG`. Thanks to Antonio Nuno Monteiro.

## [5.2.0] - 2024-08-01

### Added

- Support for MSVC compiler. Thanks to Jonah Beckford.
- `extended_errcode_int` function. Thanks to Petter A. Urkedal.
- GitHub CI. Thanks to Yilin Wei.
- `.editorconfig`.

### Fixed

- Memory allocation in `caml_sqlite3_backup_init()`. Thanks to Mark Elvers,
  Pierre Boutillier, and Benjamin Canou.
- Shadowing of `agg_ctx` when using `sizeof`.
- Switch syntax error flagged by cppcheck.

### Changed

- Detect pkgconf on Windows/mingw. Thanks to Mark Elvers, reviewed by Shon
  Feder.
- Formatted sources using `ocamlformat` and `clang-format`.
- Improved API documentation formatting.
- Enhanced README, license file, copyright notices, and changelog format.
- Removed superfluous macro conditions.

## [5.1.0] - 2021-09-22

### Added

- `let&`-operator for implicit closing of an opened database. Thanks to Yawar
  Amin <yawar.amin@gmail.com>.

## [5.0.3] - 2021-03-18

### Fixed

- Missing GC registration of init values in aggregate functions.
- Call to final aggregate function when not calling step function.
- Incorrect required OCaml version (now 4.12).

## [5.0.2] - 2020-07-30

### Added

- Missing `dune-configurator` dependency.
- Support for const char strings in stubs due to stricter handling in newer
  OCaml runtimes, eliminating C-compiler warnings.

### Changed

- Removed redundant build dependencies.
- Use `caml_alloc_initialized_string` wherever possible.
- Fixed documentation typos and wording.

## [5.0.1] - 2019-12-01

### Added

- Missing :with-test declaration in Dune project file.

### Improved

- Portability to older SQLite3 versions.

## [5.0.0] - 2019-12-01

### Breaking

- `Data.to_string` is now `Data.to_string_coerce`.

### Added

- Support for SQLite3 window functions.
- `Sqlite3.Rc.check` and `Sqlite3.Rc.is_success` for easier return code
  checking.
- `Sqlite3.prepare_or_reset` for reusing prepared statements in loops.
- `Sqlite3.iter` and `Sqlite3.fold` for more convenient handling of row data.
- More data conversion and binding functions.

### Improved

- Closing behavior of database using new SQLite3 API.
- Testing framework using `ppx_inline_test`.
- Each test case now has its own database for parallel testing.
- Compatibility with older OCaml versions. Thanks to Simon Cruanes.

### Changed

- Switched from `caml_alloc_custom` to `caml_alloc_custom_mem`.
- Switched to OPAM file generation via `dune-project`.

Thanks to Shawn <shawnw.mobile@gmail.com> and Ted Spence <tspence@fb.com>.

## [4.4.1] - 2018-10-25

### Changed

- Switched to dune, dune-release, and OPAM 2.0.

## [4.4.0] - 2018-04-26

### Added

- Support for new open flags: `uri` and `memory`. Thanks to Raman Varabets.

### Fixed

- Warnings and errors in configuration code due to upstream changes.

## [4.3.2] - 2017-11-27

### Added

- Missing -lpthread linking flag.

## [4.3.1] - 2017-11-22

### Improved

- Finalization of databases and statements for better performance.

## [4.3.0] - 2017-10-10

### Improved

- Compatibility with MSVC.

### Changed

- Used untagging and unboxing attributes on external functions.

## [4.2.0] - 2017-08-03

### Added

- Backup functionality. Thanks to Markus W. Weissmann
  <markus.weissmann@in.tum.de>.

### Changed

- Switched to jbuilder and topkg.

## Changes Before Version 4.2.0

```text
2017-06-11: Fixed return value bug in enable_load_extension. The result
            was the opposite of what the API documentation says.

2017-01-04: Added external dependency to OPAM spec for openSUSE support.

2017-01-03: Fixed incorrect LGPL license reference in Oasis specification.
            The software is actually distributed under the Expat-license.

2016-12-15: Added new functions for more efficient and convenient blob access:

              * column_blob
              * row_blobs

            Thanks to Nicolas Ojeda Bar <nicolas.ojeda.bar@lexifi.com>
            for this patch.

2016-10-07: Some portability improvements and Travis integration

            Thanks to Leonid Rozenberg <leonidr@gmail.com> for his Travis work.

2016-06-14: Changed default configuration setting for loadable extensions
            on Mac OS X. Due to frequent installation issues the default
            setting is now to turn off loadable extensions on that platform.
            You will have to explicitly turn them on if you need them.

2016-05-24: Fixed a bug finalizing user-defined functions for a database.

            Thanks to Mark Bradley <barkmadley@gmail.com> for this patch.

2015-11-18: More build process improvements for Homebrew users.

            Thanks to Leonid Rozenberg <leonidr@gmail.com> for this patch.

2015-11-05: Improved build process for Homebrew users.

            Thanks to Leonid Rozenberg <leonidr@gmail.com> for this patch.

2015-09-02: Major API change that is compatible with major release series 2:

            We can now return errors from user-defined SQL-functions by raising
            (arbitrary) exceptions. This somewhat tricky internal change
            eliminates the need for Data.ERROR and reestablishes compatibility
            with major release series 2.

            Sorry for the churn, but the more elegant solution was not obvious.

2015-08-29: Added user function error handling (major API change).

            Thanks to Joseph Young for this patch.

2015-01-29: Fixed a build problem due to Oasis/ocamlbuild inconsistency.

            Thanks to Leonid Rozenberg <leonidr@gmail.com> for this patch.

2014-10-08: Fixed a callback locking bug when encountering rows containing
            unexpected NULLs.

            Thanks to <ygrek@autistici.org> for this patch.

2014-07-04: Moved to GitHub.

2012-12-02: Added new functions

              * sleep
              * clear_bindings

            Old operating system distributions may have had problems linking
            these functions, but reasonably recent ones support them.

2012-11-19: Added missing .mldylib file for shared library support.

            Thanks to Hugo Heuzard for the bug report.

2012-07-20: Downgraded findlib version constraint to support the Debian
            testing branch.

2012-07-16: Replaced String.trim function in myocamlbuild.ml to allow
            compiling with OCaml 3.12.1.

2012-07-15: New major release version 2.0.0:

              * Upgraded to OCaml 4.00
              * Switched to Oasis for packaging
              * Switched to OCamlBuild for the build process
              * Rewrote README in Markdown
              * Added stricter compilation flags

2012-05-19: Fixed cpp warnings and removed superfluous check for dynamic
            linking library. The latter improves portability to FreeBSD.

            Thanks to Stéphane Legrand <stephleg@free.fr> for the bug report.

2011-03-10: Added sqlite3_open_v2 functionality.

            Thanks to Mike Lin <mlin@mit.edu> for the initial patch.

2010-12-20: Fixed linking problem with loadable module support.

            Thanks to Adrien Nader <camaradetux@gmail.com> for the patch.

2010-09-18: Fixed segfault related to incorrect handling of exceptions raised
            from user callbacks.

            Thanks to William Le Ferrand <wleferrand@hypios.com> for the bug
            report.

2009-12-15: Fixed segfault related to exceptions raised from C.

            Thanks to Gareth Smith <garethdanielsmith@googlemail.com> for the
            bug report.

2009-09-14: Fixed build problem.

2009-09-13: Removed deprecated functions and improved build process.

2009-09-08: Added "changes" function.

            Thanks to <ygrek@autistici.org> for this patch.

2009-08-22: enable_load_extension now raises an exception if unsupported.

2009-07-28: Added better support for compiling with MSVC and ocamlbuild.

            Thanks to <ygrek@autistici.org> for this patch.

2009-05-23: Fixed example to be consistent with new API.

2009-05-16: Fixed mishandling of OCaml-runtime lock when callbacks raise
            exceptions, and handle NULL-pointer results when getting column
            type declarations.

            Thanks to Bruno Daniel <bruno.daniel@gmx.net> for this patch.

            Changed API to expose optional results.

2009-03-09: Fixed potential build problem.

2009-03-01: Added support for user-defined aggregate functions.

            Thanks to Anil Madhavapeddy <anil@recoil.org> for the initial
            version of the patch.

2009-02-21: Added new function:

              * busy_timeout

            Thanks to Paolo Donadeo <p.donadeo@gmail.com> for the patch.

2009-01-05: Switched to generational global root registration of
            callbacks for better performance.

            Requires OCaml 3.11 or higher.

2008-12-02: Added function enable_load_extension if available.

            Thanks to Pietro Abate <Pietro.Abate@pps.jussieu.fr> for
            the patch.

2008-05-11: Added function column_count, used it internally in place of
            data_count, and improved documentation of associated
            functions.

            Thanks to James Cheney <james.cheney@gmail.com> for the patch.

2008-05-07: Renamed Data.to_string to Data.to_string_debug for converting
            fields to strings with their data constructor. Replaced the
            previous function with one that behaves more like users
            would expect. Thanks to Stefano Zacchiroli <zack@debian.org>
            for the suggestion.

2008-04-18: Improved backwards compatibility to older versions of SQLite.

2008-04-04: Fixed a build problem on Mac OS X.

2008-03-27: Fixed a build problem on FreeBSD.

            Thanks to Jaap Boender <Jaap.Boender@pps.jussieu.fr> for
            the patch.

2008-03-14: Synced with Jane Street tree.

2008-03-05: Added a patch to improve Windows support. Thanks to Alain
            Frisch <alain@frisch.fr> for the patch.

2007-09-04: Fixed a minor bug converting status codes.

2007-08-20: Fixed a GC-bug related to user-defined SQL-functions.

            Thanks to Enrico Tassi <gareuselesinge@virgilio.it> for the
            test case to replicate this problem.

2007-06-17: Added support for user-defined scalar functions. Thanks to
            Enrico Tassi <gareuselesinge@virgilio.it> for the patch.

            Switched to sqlite3_prepare_v2 internally to avoid the older,
            deprecated sqlite3_prepare-function. Thanks to Gabriel
            Kerneis <gabriel@kerneis.info> for the hint.

            Removed exec_sql-function, which was buggy in the last
            release anyway (thanks to Paul Stodghill <ps27@cornell.edu>
            for pointing this out). Its interface hides too much
            important information from the user (e.g. BUSY-steps, etc.).
            It did not seem possible to design a function that made
            it as simple as exec_sql to run an SQL-statement without
            inviting the user to write buggy/incomplete code, or that
            wouldn't make the interface almost as complicated as
            writing the correct code that handles all cases by oneself.

2007-05-07: Further GC-bug fixes. There was a design bug in the library
            that was causing all these GC issues (C-structs allocated in the
            OCaml-heap). This seemed safe (and more efficient) to the initial
            author, but after extensive checking it became obvious that this
            does not work if C-structs reference each other, because the GC
            might move these memory regions. Allocations of C-structs and
            using indirections to access them seems safer with "malloc".

2007-05-04: Fixed GC-bugs, and improved thread interaction.

2007-04-23: callback_exn -> caml_callback_exn.

2007-03-30: Fixed a GC-bug.

2007-03-19: Fixed an installation problem on Cygwin.

            Thanks to James Cheney <james.cheney@gmail.com> for the hint.

2007-02-27: Small API-change: the callback for exec is now an optional
            argument.

            Added three more "exec"-functions, which do or do not take
            headers or handle NULL-values.

            Improved quality of C-code (removed warnings).

2007-02-23: Fixed a bug in the "db_close"-function. Improved documentation.

2007-02-19: Added check for out of memory after malloc.

2007-01-31: Fixed build problem on x86_64: added -fPIC flag to compilation
            of C-stubs.

2007-01-17: Complete rewrite by Markus Mottl <markus.mottl@gmail.com>.

2005-04-xx: Initial coding (0.1) by
            Christian Szegedy <csdontspam871@metamatix.org>.
```
