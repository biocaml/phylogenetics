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

let test_felsenstein_small () =
  let mytree = TopoTree.of_string "0.21;0.1;0.3;0.4;0;0.8;0.1;1;2;0.12;0.9;3;0.2;0.3;0.3;0.4;4;5;6" in
  let myseq  = ["C";"G";"C";"T";"A";"T";"G"] |> JCFelsenstein.Align.of_string_list in
  let res1 = JCFelsenstein.felsenstein () mytree myseq in
  let res2 = JCFelsenstein.felsenstein_log () mytree myseq in
  (check @@ testable (pp float) (float_compare 0.00001)) "identical log likelihoods" res1 res2


let tests = [
  "felsenstein_tiny", `Quick, test_felsenstein_tiny ;
  "felsenstein_small", `Quick, test_felsenstein_small
]

