open Biocaml_phylogeny_core
open Alcotest
open Biocaml_ez
open Felsenstein

(** Function used to compare floats and tolerate relative imprecision.
    Returns true if (1-p)*f1 < f2 < (1+p)*f1 *)
let float_compare p f1 f2 =
  let diff = f1-.f2 |> abs_float in
  diff/.(abs_float f1) <= p

let test_felsenstein_tiny () =
  JCFelsenstein.felsenstein () (TopoTree.of_string "0.1;0.1;0;1") (JCFelsenstein.Align.of_string_list ["C";"G"]) |>
  (check @@ testable (pp float) (float_compare 0.00001)) "expected log likelihoods" (-4.22471668644312)

let tree_small = TopoTree.of_string "0.21;0.1;0.3;0.4;0;0.8;0.1;1;2;0.12;0.9;3;0.2;0.3;0.3;0.4;4;5;6"
let seq_small  = ["C";"G";"C";"T";"A";"T";"G"] |> JCFelsenstein.Align.of_string_list
let ref_small = JCFelsenstein.felsenstein () tree_small seq_small

let test_felsenstein_small_normlog () =
  JCFelsenstein.felsenstein_logshift () tree_small seq_small |>
  (check @@ testable (pp float) (float_compare 0.00001)) "identical log likelihoods" ref_small

let test_felsenstein_small_normshift () =
  JCFelsenstein.felsenstein_shift () tree_small seq_small |>
  (check @@ testable (pp float) (float_compare 0.00001)) "identical log likelihoods" ref_small

let tests = [
  "felsenstein_tiny", `Quick, test_felsenstein_tiny ;
  "felsenstein_small (normal vs logshift)", `Quick, test_felsenstein_small_normlog ;
  "felsenstein_small (normal vs shift)", `Quick, test_felsenstein_small_normshift ;
]

