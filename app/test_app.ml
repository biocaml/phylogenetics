open Phylogenetics_test

let () =
  Alcotest.run "All tests" [
    "Sequence", Test_sequence.tests;
    "Alignment", Test_alignment.tests;
    "Phylogenetic_tree", Test_topotree.tests;
    "Zipper", Test_zipper.tests;
    "Models", Test_site_evolution_model.tests;
    "Felsenstein", Test_felsenstein.tests;
    "Rejection_sampling", Test_rejection_sampling.tests;
    "MCMC", Test_MCMC.tests ;
    "Newick", Test_newick.tests ;
    "PhyloCTMC", Test_phylo_ctmc.tests ;
  ]
