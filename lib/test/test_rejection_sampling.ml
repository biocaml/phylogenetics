open Core_kernel.Std
open Alcotest
open Biocaml_phylogeny_core
open Rejection_sampling

let float_compare p f1 f2 =
  let diff = f1-.f2 |> Pervasives.abs_float in
  diff/.(Pervasives.abs_float f1) <= p

let check_distrib = (* In practice, it just compares two lists of floats. *)
  (check @@ list (testable (pp Alcotest.float) (float_compare 0.05)))
    "Distributions with identical characteristics"


(** {6 Test input parameters} *)

module RS_DNA = Make(Models.K80)
let myalign = RS_DNA.Align.of_string_list ["A";"A";"A";"T"]
let mybasetree = TopoTree.of_preorder "0.1;0.1;0.1;0.1;0;1;2.5;0.1;2;3"
let mysampler = Stat_tools.sample_branch_lengths ~branchs:(fun i -> i=5) ~sampler:(Stat_tools.sample_float_uniform 5.0) mybasetree


(** {6 Test functions} *)

let sample amount =
  let prior_trees = RS_DNA.generate_trees ~sampler:mysampler amount in
  let post_trees = RS_DNA.reject 2.0 myalign prior_trees in
  let mean_prior, mean_post, acceptance =
    (RS_DNA.mean_specific_branch 5 prior_trees),
    (RS_DNA.mean_specific_branch 5 post_trees),
    (List.length post_trees |> float_of_int) /.
    (List.length prior_trees |> float_of_int)
  in [mean_prior; mean_post; acceptance]

let test_rejection () =
  check_distrib [2.5;2.8;0.012] (sample 500000)


(** {6 Test list} *)

let tests = ["Specific branch length on small tree with 500k points.", `Slow, test_rejection]
