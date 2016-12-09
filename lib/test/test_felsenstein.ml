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
  JCFelsenstein.felsenstein () (TopoTree.of_string "0.1;0.1;0;1") (JCFelsenstein.Align.of_string_list ["C";"G"]) |> log |>
  (check @@ testable (pp float) (float_compare 0.00001)) "identical log likelihoods" (-4.22471668644312)

let tests = [
  "felsenstein_tiny", `Quick, test_felsenstein_tiny
]

