open Biocaml_phylogeny_core
open Biocaml_phylogeny_test

let _ = Random.self_init ()

let time f =
    let t = Sys.time() in
    let fexec = f () in
    Printf.printf "\027[0;31mExecution time: %fs\027[0;0m\n" (Sys.time() -. t);
    fexec

let f () =
  Test_felsenstein.test_felsenstein
    ~model:(Models.of_string "JC69").Models.model
    ~treesize:10000
    ~seqsize:1
    ~param:""
    ()

let _ = time f
