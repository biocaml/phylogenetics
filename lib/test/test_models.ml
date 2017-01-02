open Biocaml_phylogeny_core
open Alcotest

module K80_Utils = Model_utils.Model_utils (Models.K80)
module JC69_Utils = Model_utils.Model_utils (Models.JC69)
open LATools

let test_JC69_exponential () =
  (check @@ testable LATools.pp_mat (LATools.compare 0.0001))
    "identical exponential of transition matrix"
    (JC69_Utils.eMt () 0.1)
    (JC69_Utils.eMt_series () 0.1)

let test_K80_exponential () =
  (check @@ testable LATools.pp_mat (LATools.compare 0.0001))
    "identical exponential of transition matrix"
    (K80_Utils.eMt 2.0 0.1)
    (K80_Utils.eMt_series 2.0 0.1)

let tests = [
  "JC69 exponential", `Quick, test_JC69_exponential;
  "K80 exponential", `Quick, test_K80_exponential
]
