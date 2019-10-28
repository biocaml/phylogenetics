(** Tests of the felsenstein implementation. The results are compared to bppml
    to check correctness. A variety of models and problem sizes are used to generate
    trees and alignments which are submitted to our felsenstein implementation and
    bppml.*)
open Phylogenetics
open Core_kernel


(** {6 Preliminary functions} *)

(** Generates a random tree, a random sequence (using the provided model),
    runs both biocaml felsenstein and bppml, and checks that the results are identical*)
let test_felsenstein
    ?(model=(module Models.JC69:Models.S))
    ?(treesize=5)
    ?(seqsize=5)
    ?(param="")
    ()
  =
  let module M = (val model) in
  let module Align = Alignment.Make(Seq.DNA) in
  let module F = Felsenstein.Make(Nucleotide)(Align)(M) in
  let module SG = Sequence_generation.Make(Nucleotide)(Seq.DNA)(Align)(M) in
  let param = M.of_string param in
  let tree = Phylogenetic_tree.make_random treesize in
  let align =  SG.seqgen_string_list param tree seqsize |> Align.of_string_list in
  let my_result = F.felsenstein param tree align in
  let bpp_result = begin
    Phylogenetic_tree.to_newick_file tree "tmp.tree" ; (* TODO unique file name *)
    Align.to_file align "tmp.seq" ;
    try
      Test_utils.felsenstein_bpp ~model:(Printf.sprintf "\"%s\"" (M.to_string param)) ~tree:("tmp.tree") "tmp.seq"
    with
    | Failure s -> Printf.printf "\027[0;31mERROR\027[0;0m(felsenstein_bpp): %s\n" s; 0.0
  end in
  Test_utils.check_likelihood my_result bpp_result

(** Wrapper for test_felsenstein that uses the string to model identification using bpp format *)
let test_felsenstein_str ?(model="JC69") ?(treesize=5) ?(seqsize=5) () =
  let my_model = Models.of_string model in
  Option.iter my_model ~f:(fun model ->
      test_felsenstein ~model:model.Models.model ~treesize ~seqsize ~param:model.Models.param ()
    )


(** {6 Test list} *)

let models = ["JC69" ; "K80(kappa=2.0)" ; "K80(kappa=0.5)" ; "JC69_generated" ; "K80_generated(kappa=2.0)"]
let tree_sizes = [10 ; 250]
let seq_sizes = [1 ; 100 ]

let tests =
  List.cartesian_product tree_sizes seq_sizes
  |> List.cartesian_product models
  |> List.map ~f:(fun (model, (treesize, seqsize)) ->
      (Printf.sprintf "Biocaml vs bppml\t%s\ttreesize=%d\tseqsize=%d" model treesize seqsize,
       `Slow, test_felsenstein_str ~model:model ~treesize ~seqsize)
    )