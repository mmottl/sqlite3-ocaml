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
