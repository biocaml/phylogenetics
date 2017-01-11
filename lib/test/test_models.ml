open Biocaml_phylogeny_core
open Alcotest

module K80_Utils = Models.K80
module K80_gen_Utils = Models.K80_generated
module JC69_Utils = Models.JC69
module JC69_gen_Utils = Models.JC69_generated
open LATools

let compare_matrix = check @@ testable LATools.pp_mat (LATools.compare 0.0001)


let test_JC69_exponential () =
  compare_matrix
    "identical exponential of transition matrix"
    (JC69_Utils.eMt_mat () 0.1)
    (JC69_Utils.eMt_series () 0.1)

let test_K80_exponential () =
  compare_matrix
    "identical exponential of transition matrix"
    (K80_Utils.eMt_mat 2.0 0.1)
    (K80_Utils.eMt_series 2.0 0.1)

let test_JC69_generated () =
  compare_matrix
    "identical exponential of transition matrix"
    (JC69_Utils.eMt_mat () 0.1)
    (JC69_gen_Utils.eMt_mat () 0.1)

let test_K80_generated () =
  compare_matrix
    "identical exponential of transition matrix"
    (K80_Utils.eMt_mat 2.0 0.1)
    (K80_gen_Utils.eMt_mat 2.0 0.1)

let tests = [
  "JC69 exponential", `Quick, test_JC69_exponential;
  "K80 exponential", `Quick, test_K80_exponential;
  "JC69 manual vs generated", `Quick, test_JC69_generated;
  "K80 manual vs generated", `Quick, test_K80_generated
]
