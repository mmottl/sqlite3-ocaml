let exn_protect ~finally x ~f =
  try
    let y = f x in
    finally x;
    y
  with e ->
    finally x;
    raise e

let read_lines_from_cmd ~max_lines cmd =
  let ic =
    try Unix.open_process_in cmd
    with exc ->
      Printf.eprintf "read_lines_from_cmd: could not open cmd: '%s'" cmd;
      raise exc
  in
  exn_protect ic ~finally:close_in_noerr ~f:(fun ic ->
    let rec loop n lines =
      if n <= 0 then List.rev lines
      else
        match input_line ic with
        | line -> loop (n - 1) (line :: lines)
        | exception _ ->
            Printf.eprintf "read_lines_from_cmd: failed reading line %d, cmd: '%s'"
              (max_lines - n + 1) cmd;
            raise End_of_file
    in
    loop max_lines [])

let opt_map ~default ~f x = match x with
  | Some y -> f y
  | None -> default
let opt_is_some = function Some _ -> true | _ -> false
let getenv_opt s = try Some (Sys.getenv s) with _ -> None

let pkg_export =
  let has_brewcheck =
    opt_is_some (getenv_opt "SQLITE3_OCAML_BREWCHECK")
  in
  if not has_brewcheck then ""
  else
    let cmd = "brew ls sqlite | grep pkgconfig" in
    match read_lines_from_cmd ~max_lines:1 cmd with
    | [fullpath] when not (String.equal fullpath "") ->
        let path = Filename.dirname fullpath in
        Printf.sprintf "PKG_CONFIG_PATH=%s" path
    | _ -> ""

let split_ws str =
  let l = ref [] in
  let i = ref 0 in
  while !i < String.length str do
    let j = String.index_from str !i ' ' in
    if !i=j then incr i
    else (
      l := String.sub str !i (j- !i) :: !l;
      i := j+1;
    )
  done;
  List.rev !l

let () =
  let module C = Configurator.V1 in
  C.main ~name:"sqlite3" (fun c ->
    let is_macosx =
      opt_map (C.ocaml_config_var c "system") ~default:false
        ~f:(function "macosx" -> true | _ -> false)
    in
    let cflags =
      let cmd = pkg_export ^ " pkg-config --cflags sqlite3" in
      match read_lines_from_cmd ~max_lines:1 cmd with
      | [cflags] ->
          let cflags = split_ws cflags in
          if
            is_macosx ||
            opt_is_some (
              getenv_opt "SQLITE3_DISABLE_LOADABLE_EXTENSIONS")
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
    C.Flags.write_sexp "c_flags.sexp" conf.cflags;
    C.Flags.write_sexp "c_library_flags.sexp" conf.libs)
