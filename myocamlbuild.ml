open Ocamlbuild_plugin
open Command

let static = true

(*
let includes = "-IC:/my/contrib/sqlite-3_6_16"
let lib = "C:/my/contrib/sqlite-3_6_16/sqlite3.lib"
*)

let includes = ""
let lib = "-lsqlite3"

;;

dispatch begin function
| Before_options ->

     if Sys.os_type = "Win32" then
     begin
       Options.ext_lib := "lib";
       Options.ext_obj := "obj";
       Options.ext_dll := "dll"
     end

| After_rules ->

     ocaml_lib "sqlite3";

     let stubs = "libsqlite3_stubs." ^ !Options.ext_lib in
     flag ["link"; "library"; "ocaml"] (S[A"-cclib"; A stubs; A"-cclib"; A lib]);
     flag ["c"; "compile"] (S[A"-verbose";A"-ccopt"; A includes;]);
     if static then flag ["link"; "ocaml"; "byte"] (A"-custom");
     dep  ["link"; "ocaml";"library"] [stubs];
     ()
 
| _ -> ()
end
