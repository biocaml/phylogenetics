open Biocaml_phylogeny_core
open Biocaml_phylogeny_test

let _ = Random.self_init ()

(* let time f = *)
(*   let t = Sys.time() in *)
(*   let fexec = f () in *)
(*   Printf.printf "\027[0;32mExecution time: %fs\027[0;0m\n" (Sys.time() -. t); *)
(*   fexec *)

(* let f () = *)
(*   try *)
(*     Test_felsenstein.test_felsenstein_str *)
(*       ~model:"JC69" *)
(*       ~treesize:3000 *)
(*       ~seqsize:1 *)
(*       () *)
(*   with *)
(*     Failure x -> Printf.printf "\027[0;31mERROR\027[0;0m(test_felsenstein): %s\n" x *)

(* let _ = time f *)

let _ = Rejection_sampling.test 1000000
