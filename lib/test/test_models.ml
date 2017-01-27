open Biocaml_phylogeny_core
open Alcotest
open Linear_algebra_tools
open Models

let compare_matrix = check @@ testable pp_mat (compare 0.0001)

let test_JC69_exponential () =
  compare_matrix
    "identical exponential of transition matrix"
    (JC69.eMt_mat () 0.1)
    (JC69.eMt_series () 0.1)

let test_K80_exponential () =
  compare_matrix
    "identical exponential of transition matrix"
    (K80.eMt_mat 2.0 0.1)
    (K80.eMt_series 2.0 0.1)

let test_JC69_generated () =
  compare_matrix
    "identical exponential of transition matrix"
    (JC69.eMt_mat () 0.1)
    (JC69_generated.eMt_mat () 0.1)

let test_K80_generated () =
  compare_matrix
    "identical exponential of transition matrix"
    (K80.eMt_mat 2.0 0.1)
    (K80_generated.eMt_mat 2.0 0.1)

let tests = [
  "JC69 exponential", `Quick, test_JC69_exponential;
  "K80 exponential", `Quick, test_K80_exponential;
  "JC69 manual vs generated", `Quick, test_JC69_generated;
  "K80 manual vs generated", `Quick, test_K80_generated
]
