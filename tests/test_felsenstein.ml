(** Tests of the felsenstein implementation. The results are compared to bppml
    to check correctness. A variety of models and problem sizes are used to generate
    trees and alignments which are submitted to our felsenstein implementation and
    bppml.*)
open Phylogenetics
open Core_kernel


type 'a model = (module Site_evolution_model.Nucleotide_S_with_reduction with type param = 'a)

type test_case = Test_case : {
    model : 'a model ;
    param : 'a ;
    bpp_spec : Bpp_model.t ;
  } -> test_case

(** {6 Preliminary functions} *)

(** Generates a random tree, a random sequence (using the provided model),
    runs both biocaml felsenstein and bppml, and checks that the results are identical*)
let test_felsenstein ?(treesize=5) ?(seqsize=5) (Test_case c) () =
  let module M = (val c.model) in
  let module Align = Alignment.Make(Seq.DNA) in
  let module F = Felsenstein.Make(Nucleotide)(Align)(M) in
  let module SG = Sequence_generation.Make(Nucleotide)(Seq.DNA)(Align)(M) in
  let tree = Phylogenetic_tree.make_random treesize in
  let align =  SG.seqgen_string_list c.param tree seqsize |> Align.of_string_list in
  let my_result = F.felsenstein c.param tree align in
  let bpp_result = begin
    Phylogenetic_tree.to_newick_file tree "tmp.tree" ; (* TODO unique file name *)
    Align.to_file align "tmp.seq" ;
    try
      Test_utils.felsenstein_bpp ~model:(Printf.sprintf "\"%s\"" (Bpp_model.to_string c.bpp_spec)) ~tree:("tmp.tree") "tmp.seq"
    with
    | Failure s -> Printf.printf "\027[0;31mERROR\027[0;0m(felsenstein_bpp): %s\n" s; 0.0
  end in
  Test_utils.check_likelihood my_result bpp_result


(** {6 Test list} *)

let models = Site_evolution_model.[
  Test_case { model = (module JC69) ; param = () ; bpp_spec = JC69 } ;
  Test_case { model = (module K80) ; param = 2. ; bpp_spec = K80 2. } ;
  Test_case { model = (module K80) ; param = 0.5 ; bpp_spec = K80 0.5 } ;
  Test_case { model = (module JC69_numerical) ; param = () ; bpp_spec = JC69 } ;
  Test_case { model = (module K80_numerical) ; param = 4. ; bpp_spec = K80 4. } ;
]

let tree_sizes = [10 ; 250]
let seq_sizes = [1 ; 100 ]

let tests =
  List.cartesian_product tree_sizes seq_sizes
  |> List.cartesian_product models
  |> List.map ~f:(fun ((Test_case tc as test_case), (treesize, seqsize)) ->
      (Printf.sprintf "Biocaml vs bppml\t%s\ttreesize=%d\tseqsize=%d" (Bpp_model.to_string tc.bpp_spec) treesize seqsize,
       `Slow, test_felsenstein ~treesize ~seqsize test_case)
    )
