open Base
open Stdio

let read_lines_from_cmd ~max_lines cmd =
  let ic =
    try Unix.open_process_in cmd
    with exc ->
      eprintf "read_lines_from_cmd: could not open cmd: '%s'" cmd;
      raise exc
  in
  Exn.protectx ic ~finally:In_channel.close ~f:(fun ic ->
    let rec loop n lines =
      if n <= 0 then List.rev lines
      else
        match In_channel.input_line ic with
        | Some line -> loop (n - 1) (line :: lines)
        | None ->
            eprintf "read_lines_from_cmd: failed reading line %d, cmd: '%s'"
              (max_lines - n + 1) cmd;
            raise End_of_file
    in
    loop max_lines [])

let pkg_export =
  let has_brewcheck =
    try ignore (Caml.Sys.getenv "SQLITE3_OCAML_BREWCHECK"); true
    with _ -> false
  in
  if not has_brewcheck then ""
  else
    let cmd = "brew ls sqlite | grep pkgconfig" in
    match read_lines_from_cmd ~max_lines:1 cmd with
    | [fullpath] when String.(fullpath <> "") ->
        let path = Caml.Filename.dirname fullpath in
        Printf.sprintf "PKG_CONFIG_PATH=%s" path
    | _ -> ""

let split_ws str = List.filter (String.split ~on:' ' str) ~f:(String.(<>) "")

let () =
  let module C = Configurator in
  C.main ~name:"sqlite3" (fun c ->
    let is_macosx =
      Option.value_map (C.ocaml_config_var c "system") ~default:false
        ~f:(function "macosx" -> true | _ -> false)
    in
    let cflags =
      let cmd = pkg_export ^ " pkg-config --cflags sqlite3" in
      match read_lines_from_cmd ~max_lines:1 cmd with
      | [cflags] ->
          let cflags = split_ws cflags in
          if
            is_macosx ||
            Option.is_some (
              Caml.Sys.getenv_opt "SQLITE3_DISABLE_LOADABLE_EXTENSIONS")
          then "-DSQLITE3_DISABLE_LOADABLE_EXTENSIONS" :: cflags
          else cflags
      | _ -> failwith "pkg-config failed to return cflags"
    in
    let libs =
      let cmd = pkg_export ^ " pkg-config --libs sqlite3" in
      match read_lines_from_cmd ~max_lines:1 cmd with
      | [libs] -> split_ws libs
      | _ -> failwith "pkg-config failed to return libs"
    in
    let conf = { C.Pkg_config.cflags; libs } in
    let write_sexp file sexp =
      Out_channel.write_all file ~data:(Sexp.to_string sexp)
    in
    write_sexp "c_flags.sexp" (sexp_of_list sexp_of_string conf.cflags);
    write_sexp "c_library_flags.sexp" (sexp_of_list sexp_of_string conf.libs))
