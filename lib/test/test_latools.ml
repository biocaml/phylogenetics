open Biocaml_phylogeny_core
open Alcotest

(** Function used to compare floats and tolerate relative imprecision.
    Returns true if (1-p)*f1 < f2 < (1+p)*f1 *)
let float_compare p f1 f2 =
  let diff = f1-.f2 |> abs_float in
  diff/.(abs_float f1) <= p

(* TODO currently non-functional *)
let test_exp () =
  (check @@ testable LATools.pp_mat (=)) "correct exponential"

let tests = [
  "exp", `Quick, test_exp
]
