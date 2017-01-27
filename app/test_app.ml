open Alcotest
open Biocaml_phylogeny_test

let () =
  Alcotest.run "All tests" [
    "Sequence", Test_sequence.tests;
    "Alignment", Test_alignment.tests;
    "TopoTree", Test_topotree.tests;
    "Models", Test_models.tests;
    "Felsenstein", Test_felsenstein.tests;
    "Rejection_sampling", Test_rejectionsampling.tests
  ]
