open Core_kernel
open Phylogenetics
open Alcotest


(** {6 Test input parameters} *)

module DNA = Seq.DNA

let myseq = DNA.of_list Nucleotide.[a;g;c;t]


(** {6 Test functions} *)

let test_of_string () =
  DNA.of_string_exn "AGCT" |>
  (check @@ testable DNA.pp Poly.equal) "identical sequences" myseq


(** {6 Test list} *)

let tests = [
  "of_string", `Quick, test_of_string
]
