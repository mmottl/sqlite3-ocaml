open Base

let () =
  let module C = Configurator.V1 in
  C.main ~name:"sqlite3" (fun c ->
    let is_macosx =
      Option.value_map (C.ocaml_config_var c "system") ~default:false
        ~f:(function "macosx" -> true | _ -> false)
    in
    let is_windows =
      Option.value_map (C.ocaml_config_var c "system") ~default:false
        ~f:(function "win32" | "win64" -> true | _ -> false)
      in
    let is_msvc =
      Option.value_map (C.ocaml_config_var c "ccomp_type") ~default:false
        ~f:(function "msvc" -> true | _ -> false)
    in
    let cflags =
      let cflags =
        if is_msvc
        then ["-Zi"; "-Ox"; "-FS"]
        else ["-g"; "-O2"; "-fPIC"; "-DPIC"] in
      if
        is_macosx ||
        Option.is_some (
          Caml.Sys.getenv_opt "SQLITE3_DISABLE_LOADABLE_EXTENSIONS")
      then "-DSQLITE3_DISABLE_LOADABLE_EXTENSIONS" :: cflags
      else cflags
    in
    let libs =
      if is_windows
      then []
      else ["-lpthread"]
    in
    let conf = { C.Pkg_config.cflags; libs } in
    C.Flags.write_sexp "c_flags.sexp" conf.cflags;
    C.Flags.write_sexp "c_library_flags.sexp" conf.libs)
