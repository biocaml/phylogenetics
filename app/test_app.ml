open Biocaml_phylogeny
open Alcotest

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
    let tmp = table_of_string_list ["ATT";"TGC";"GTC"] in
    (check bool) "identical sequence tables" true (mytab = tmp)

  let test_seq_of_string () =
    let tmp = seq_of_string "AGCT" in
    (check bool) "identical sequences" true (myseq = tmp)

  let test_get_base () =
    get_base 2 2 mytab |> string_of_base |>
    (check string) "get base from sequence" "C"

  let tests = [
    "get_base", `Quick, test_get_base ;
    "table_of_string_list", `Quick, test_table_of_string_list ;
    "seq_of_string", `Quick, test_seq_of_string
  ]
end

let () =
  Alcotest.run "All tests" [
    "Sequence", Test_Sequence.tests
  ]
