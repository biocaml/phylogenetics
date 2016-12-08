open Biocaml_phylogeny_core
open Alcotest

module DNA = Sequence.DNA
module Align = Alignment.Make (DNA)

(* Reference data for tests (hand-crafted from raw DNA bases) *)
let mytab = Align.of_assoc_list Nucleotide.[(0,DNA.of_list [A;T;T]);
                                            (1,DNA.of_list [T;G;C]);
                                            (2,DNA.of_list [G;T;C])]

let test_of_string_list () =
  Align.of_string_list ["ATT";"TGC";"GTC"] |>
  (check @@ testable Align.pp (=)) "identical sequence tables" mytab

let test_of_fasta () =
  Align.of_fasta "test_data/tiny1.fasta" |>
  (check @@ testable Align.pp (=)) "identical sequence tables" mytab

let test_get_base () =
  Align.get_base ~seq:2 ~pos:2 mytab |> Nucleotide.to_string |>
  (check string) "get base from sequence" "C"

let tests = [
  "of_string_list", `Quick, test_of_string_list ;
  "of_fasta", `Quick, test_of_fasta ;
  "get_base", `Quick, test_get_base
]

