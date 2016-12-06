open Biocaml_phylogeny
open Alcotest

(** Function used to compare floats and tolerate relative imprecision.
    Returns true if (1-p)*f1 < f2 < (1+p)*f1 *)
let float_compare p f1 f2 =
  let diff = f1-.f2 |> abs_float in
  diff/.(abs_float f1) <= p

module Test_Sequence = struct
  open Sequence
  open DNA_Sequence

  (* Reference data for tests (hand-crafted from raw DNA bases) *)
  let mytab = [(0,[A;T;T]);
               (1,[T;G;C]);
               (2,[G;T;C])]

  let myseq = [A;G;C;T]

  (* Test functions *)
  let test_table_of_string_list () =
    table_of_string_list ["ATT";"TGC";"GTC"] |>
    (check @@ testable pp_table (=)) "identical sequence tables" mytab

  let test_seq_of_string () =
    seq_of_string "AGCT" |>
    (check @@ testable pp_seq (=)) "identical sequences" myseq

  let test_get_base () =
    get_base 2 2 mytab |> string_of_base |>
    (check string) "get base from sequence" "C"

  let tests = [
    "get_base", `Quick, test_get_base ;
    "table_of_string_list", `Quick, test_table_of_string_list ;
    "seq_of_string", `Quick, test_seq_of_string
  ]
end

module Test_Felsenstein = struct
  open Models

  let test_felsenstein_tiny () =
    JCFelsenstein.felsenstein (TopoTree.tree_of_string "0.1;0.1;0;1") (JCFelsenstein.table_of_string_list ["C";"G"]) |> log |>
    (check @@ testable (pp float) (float_compare 0.00001)) "identical sequences" (-4.22471668644312)

  let tests = [
    "felsenstein_tiny", `Quick, test_felsenstein_tiny
  ]

end

let () =
  Alcotest.run "All tests" [
    "Sequence", Test_Sequence.tests;
    "Felsenstein", Test_Felsenstein.tests;
  ]
