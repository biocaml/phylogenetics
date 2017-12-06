open Core_kernel.Std
open Biocaml_phylogeny
open Alcotest


(** {6 Test input parameters} *)

module DNA = Seq.DNA

let myseq = DNA.of_list Nucleotide.[A;G;C;T]


(** {6 Test functions} *)

let test_of_string () =
  DNA.of_string "AGCT" |>
  (check @@ testable DNA.pp (=)) "identical sequences" myseq


(** {6 Test list} *)

let tests = [
  "of_string", `Quick, test_of_string
]
