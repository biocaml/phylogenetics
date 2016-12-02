open Biocaml_phylogeny
open Alcotest

(** Sequence.seq_of_string *)
let test_Sequence_seq_of_string () =
  let open Sequence.DNA_Sequence in
  let aux =
    let myseq = table_of_string_list ["ATT";"TGC";"GTC"] in
    let myseq2 = [(0,List.map base_of_int [0;3;3]);
                  (1,List.map base_of_int [3;2;1]);
                  (2,List.map base_of_int [2;3;1])]
    in
    myseq2 = myseq
  in
  (check bool) "get base from sequence" true aux

(** Sequence.get_base *)
let test_Sequence_get_base () =
  let open Sequence.DNA_Sequence in
  let aux =
    let myseq = table_of_string_list ["ATT";"TGC";"GTC";"CGT"] in
    get_base 2 2 myseq |> string_of_base
  in
  (check string) "get base from sequence" "C" aux

let seq_tests = [
  "get_base", `Quick, test_Sequence_get_base ;
  "seq_of_string", `Quick, test_Sequence_seq_of_string
]

let () =
  Alcotest.run "All tests" [
    "Sequence", seq_tests
  ]
