open Phylogenetics
open Test_utils
open Site_evolution_model


let test_JC69_exponential () =
  compare_matrices
    (module Nucleotide)
    "JC69: numerical vs analytical exponential"
    (JC69.transition_probability_matrix () 0.1)
    (JC69_numerical.transition_probability_matrix () 0.1)

let test_K80_exponential () =
  compare_matrices
    (module Nucleotide)
    "K80: numerical vs analytical exponential"
    (K80.transition_probability_matrix 2.0 0.1)
    (K80_numerical.transition_probability_matrix 2.0 0.1)

let tests = [
  "JC69 exponential", `Quick, test_JC69_exponential;
  "K80 exponential", `Quick, test_K80_exponential;
]
