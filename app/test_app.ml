open Alcotest
open Biocaml_phylogeny_test

let () =
  Alcotest.run "All tests" [
    "Sequence", Test_sequence.tests;
    "Alignment", Test_alignment.tests;
    "Phylogenetic_tree", Test_topotree.tests;
    "Zipper", Test_zipper.tests;
    "Models", Test_models.tests;
    "Felsenstein", Test_felsenstein.tests;
    "Rejection_sampling", Test_rejection_sampling.tests;
    "MCMC", Test_MCMC.tests
  ]
