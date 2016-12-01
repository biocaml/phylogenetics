(* #require "lacaml" *)
(* open Lacaml.S;; *)

(* let mat = Mat.random 4 4;; *)

open Biocaml_phylogeny
open Alcotest

let seq_test () =
  (check string) "get base from sequence" "C" (Sequence.test ())

let seq_tests = [
  "get", `Quick, seq_test
]

let () =
  Alcotest.run "All tests" [
    "sequence", seq_tests
  ]

(* TopoTree.test () ;; *)
(* Models.test () ;; *)
(* LATools.test () ;; *)
(* LATools.exptest () ;; *)
