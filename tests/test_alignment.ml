open Core
open Phylogenetics
open Alcotest


(** {6 Test input parameters} *)

module DNA = Seq.DNA
module Align = Alignment.Make (DNA)

let mytab = Align.of_assoc_list Nucleotide.[("T0",DNA.of_list [a;t;t]);
                                            ("T1",DNA.of_list [t;g;c]);
                                            ("T2",DNA.of_list [g;t;c])]


(** {6 Test functions} *)

let test_of_string_list () =
  Align.of_string_list ["ATT";"TGC";"GTC"] |>
  (check @@ testable Align.pp Align.equal) "identical sequence tables" mytab

let test_of_fasta_with_header () =
  Align.of_fasta "../tests/data/tiny1.fasta" |>
  (check @@ testable Align.pp Align.equal) "identical sequence tables" mytab

let test_of_fasta () =
  Align.of_fasta "../tests/data/tiny2.fasta" |>
  (check @@ testable Align.pp Align.equal) "identical sequence tables" mytab


let test_get_base () =
  Align.get_base ~seq:"T1" ~pos:2 mytab
  |> Nucleotide.to_char
  |> check char "get base from sequence" 'C'


(** {6 Test list} *)

let tests = [
  "of_string_list", `Quick, test_of_string_list ;
  "of_fasta", `Quick, test_of_fasta ;
  "get_base", `Quick, test_get_base
]

