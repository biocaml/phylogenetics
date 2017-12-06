open Core_kernel.Std
open Alcotest
open Biocaml_phylogeny
open Rejection_sampling


(** {6 Test input parameters} *)

module RS_DNA = Make(Models.K80)
let myalign = RS_DNA.Align.of_string_list ["A"; "A"; "A"; "T"]
let mybasetree = Phylogenetic_tree.of_preorder "0.1;0.1;0.1;0.1;0;1;2.5;0.1;2;3"
let mysampler = Stat_tools.sample_branch_lengths ~branchs:(fun i -> i=5)
    ~sampler:(Stat_tools.sample_float_uniform 5.0) mybasetree


(** {6 Test functions} *)

let sample amount =
  let prior_trees = RS_DNA.generate_trees ~sampler:mysampler amount in
  let post_trees = RS_DNA.reject 2.0 myalign prior_trees in
  List.map post_trees ~f:(fun t ->
      Phylogenetic_tree.get_branch_lengths t
      |> fun l -> List.nth_exn l 5
    )

let test_rejection () =
  Test_utils.check_distrib [2.8] (sample 500000)


(** {6 Test list} *)

let tests = [
  "Specific branch length on tiny tree with 500k points.", `Slow, test_rejection]
