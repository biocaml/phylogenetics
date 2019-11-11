open Core_kernel
open Phylogenetics

let test_pruning ?(tree_size = 5) ?(seq_size = 10) () =
  let module M = Site_evolution_model.JC69 in
  let module Align = Alignment.Make(Seq.DNA) in
  let module F = Felsenstein.Make(Nucleotide)(Align)(M) in
  let module SG = Sequence_generation.Make(Nucleotide)(Seq.DNA)(Align)(M) in
  let module CTMC = Phylo_ctmc.Make(Nucleotide) in
  let tree = Phylogenetic_tree.make_random tree_size in
  let align =
    SG.seqgen_string_list () tree seq_size
    |> Align.of_string_list
  in
  let felsenstein_result = F.felsenstein () tree align in
  let ctmc_result =
    let tree = Phylogenetic_tree.to_tree tree in
    let transition_matrix l = M.eMt_mat () l in
    let root_frequencies = M.stat_dist_vec () in
    Array.init (Align.length align) ~f:(fun i ->
        let leaf_state (_, index) = Align.get_base align ~seq:index ~pos:i in
        CTMC.pruning tree ~transition_matrix ~leaf_state ~root_frequencies
      )
    |> Owl.Stats.sum
  in
  Test_utils.check_likelihood felsenstein_result ctmc_result

let tests = [
  ("Felsenstein vs Phylo_ctmc", `Slow, test_pruning ~tree_size:100 ~seq_size:10) ;
]
