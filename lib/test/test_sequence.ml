open Biocaml_phylogeny_core
open Alcotest

module DNA = Sequence.DNA

let myseq = DNA.of_list Nucleotide.[A;G;C;T]

(* Test functions *)
let test_of_string () =
  DNA.of_string "AGCT" |>
  (check @@ testable DNA.pp (=)) "identical sequences" myseq

let tests = [
  "of_string", `Quick, test_of_string
]
