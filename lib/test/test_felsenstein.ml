open Biocaml_phylogeny_core
open Alcotest
open Biocaml_ez
open Core_kernel.Std

(** Function used to compare floats and tolerate relative imprecision.
    Returns true if (1-p)*f1 < f2 < (1+p)*f1 *)
let float_compare p f1 f2 =
  let diff = f1-.f2 |> Pervasives.abs_float in
  diff/.(Pervasives.abs_float f1) <= p

(** Compares two floats (which are supposed to be likelihood results) using the alcotest check *)
let check_likelihood = (check @@ testable (pp Alcotest.float) (float_compare 0.00001)) "different log likelihoods!"

(** Generates a random tree, a random sequence (using the provided model),
    runs both biocaml felsenstein and bppml, and checks that the results are identical*)
let test_felsenstein
    ?(model=(module Models.JC69:Sigs.EVOL_MODEL))
    ?(treesize=5)
    ?(seqsize=5)
    ?(param="")
    ()
  =
  let module M = (val model) in
  let module F = Felsenstein.Felsenstein (M) in
  let module SG = Seqgen.Seqgen (M) in
  let param = M.of_string param in
  let tree = TopoTree.make_random treesize in
  let align =  SG.seqgen_string_list param tree seqsize |> F.Align.of_string_list in
  let my_result = F.multi_felsenstein_shift () param tree align in
  let bpp_result = begin
    TopoTree.to_newick_file tree "tmp.tree" ; (* TODO unique file name *)
    F.Align.to_file align "tmp.seq" ;
    Bpp_interface.felsenstein_bpp ~model:(Printf.sprintf "\"%s\"" (M.to_string param)) ~tree:("tmp.tree") "tmp.seq"
  end in
  check_likelihood my_result bpp_result

(** Wrapper for test_felsenstein that uses the string to model identification using bpp format *)
let test_felsenstein_str ?(model="JC69") ?(treesize=5) ?(seqsize=5) =
  let my_model = Models.of_string model in
  test_felsenstein ~model:my_model.Models.model ~treesize ~seqsize ~param:my_model.Models.param


(* TESTS *)
let models = ["JC69" ; "K80(kappa=2.0)" ; "K80(kappa=0.5)"]
let tree_sizes = [10 ; 50 ; 250]
let seq_sizes = [1 ; 10 ; 100 ]

let tests =
  List.cartesian_product tree_sizes seq_sizes
  |> List.cartesian_product models
  |> List.map ~f:(fun (model, (treesize, seqsize)) ->
      (Printf.sprintf "Biocaml vs bppml\t%s\ttreesize=%d\tseqsize=%d" model treesize seqsize,
       `Slow, test_felsenstein_str ~model:model ~treesize ~seqsize)
    )
