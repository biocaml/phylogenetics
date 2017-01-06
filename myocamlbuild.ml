open Printf
open Solvuu_build.Std
open Solvuu_build.Util

let project_name = "biocaml_phylogeny"
let version = "dev"

let annot = ()
let bin_annot = ()
let g = ()
let short_paths = ()
let thread = ()

let undash = String.map (function '-' -> '_' | c -> c)

let make_lib ?findlib_deps ?internal_deps ?ml_files ?mli_files lib_name =
  let wrap_name = sprintf "%s_%s" project_name lib_name in
  Project.lib wrap_name
    ~thread
    ?findlib_deps ?internal_deps
    ?ml_files
    ?mli_files
    ~dir:(sprintf "lib/%s" lib_name)
    ~style:(`Pack wrap_name)
    ~pkg:(sprintf "%s.%s" project_name lib_name)

let make_app = Project.app ~thread

let core_lib = make_lib "core"
    ~findlib_deps:["lacaml" ; "biocaml.ez" ; "core_kernel"]
    ~ml_files:(`Add ["newick_lexer.ml" ; "newick_parser.ml"])
    ~mli_files:(`Add ["newick_parser.mli"])

let test_lib = make_lib "test"
    ~findlib_deps:["alcotest"]
    ~internal_deps:[core_lib]

let test_app = make_app "test_app"
    ~file:"app/test_app.ml"
    ~internal_deps:[test_lib]

let bench_app = make_app "bench_app"
    ~file:"app/bench_app.ml"
    ~internal_deps:[test_lib; core_lib]

(* let lib = *)
(*   let name = "biocaml_phylogeny" in *)
(*   Project.lib name *)
(*     ~annot ~bin_annot ~g ~short_paths ~thread *)
(*     ~pkg:name *)
(*     ~dir:"lib" *)
(*     ~style:(`Pack name) *)
(*     ~findlib_deps:[ *)
(*       "lacaml, alcotest" ; *)
(*     ] *)

(* let app = *)
(*   let name = "test" in *)
(*   Project.app name *)
(*     ~annot ~bin_annot ~g ~short_paths ~thread *)
(*     ~file:(sprintf "app/%s_app.ml" (undash name)) *)
(*     ~internal_deps:[lib_tests] *)

let items = [ test_app ; bench_app ; test_lib ; core_lib ]


let () =
  let open Solvuu_build.Std.Project in
  solvuu1 ~project_name ~version items
