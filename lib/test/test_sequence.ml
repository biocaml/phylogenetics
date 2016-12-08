open Biocaml_phylogeny_core
open Alcotest

module Align = Alignment.Make (Sequence.DNA)
module DNA = Sequence.DNA

(* Reference data for tests (hand-crafted from raw DNA bases) *)
let mytab = Align.of_assoc_list Nucleotide.[(0,DNA.of_list [A;T;T]);
                                            (1,DNA.of_list [T;G;C]);
                                            (2,DNA.of_list [G;T;C])]

let myseq = DNA.of_list Nucleotide.[A;G;C;T]

(* Test functions *)
let test_table_of_string_list () =
  Align.of_string_list ["ATT";"TGC";"GTC"] |>
  (check @@ testable Align.pp (=)) "identical sequence tables" mytab

let test_seq_of_string () =
  DNA.of_string "AGCT" |>
  (check @@ testable DNA.pp (=)) "identical sequences" myseq

let test_get_base () =
  Align.get_base ~seq:2 ~pos:2 mytab |> Nucleotide.to_string |>
  (check string) "get base from sequence" "C"

let tests = [
  "get_base", `Quick, test_get_base ;
  "table_of_string_list", `Quick, test_table_of_string_list ;
  "seq_of_string", `Quick, test_seq_of_string
]
