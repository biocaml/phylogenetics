open Biocaml_phylogeny_core
open Alcotest
open Biocaml_ez
open Core_kernel.Std
open Felsenstein.JC69Felsenstein

(** Function used to compare floats and tolerate relative imprecision.
    Returns true if (1-p)*f1 < f2 < (1+p)*f1 *)
let float_compare p f1 f2 =
  let diff = f1-.f2 |> Pervasives.abs_float in
  diff/.(Pervasives.abs_float f1) <= p

let check_likelihood = (check @@ testable (pp Alcotest.float) (float_compare 0.00001)) "identical log likelihoods"

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

let test_felsenstein_str ?(model="JC69") ?(treesize=5) ?(seqsize=5) =
  let my_model = Models.of_string model in
  test_felsenstein ~model:my_model.Models.model ~treesize ~seqsize ~param:my_model.Models.param


(* TESTS *)
let models = ["JC69" ; "K80(kappa=2.0)" ; "K80(kappa=0.5)"]
let tree_sizes = [10 ; 50 ; 250]
let seq_sizes = [1 ; 20 ; 400 ]

let tests =
  List.cartesian_product tree_sizes seq_sizes
  |> List.cartesian_product models
  |> List.map ~f:(fun (model, (treesize, seqsize)) ->
      (Printf.sprintf "Comparison to bppml ; model=%s ; treesize=%d ; seqsize=%d" model treesize seqsize,
      `Quick, test_felsenstein_str ~model:model ~treesize ~seqsize)
  )

(* type test_case = { *)
(*   name: string ; *)
(*   result: float ; *)
(*   tree: TopoTree.t ; *)
(*   seq: Align.t ; *)
(* } *)

(* let read_test_file path file = *)
(*   let open Core_kernel.Std.In_channel in *)
(*   let rec aux path acc = function *)
(*     | name::range::models::t -> *)
(*       let new_acc = list_cases path name *)
(*           (Scanf.sscanf range "%d-%d" (fun x y->x,y)) *)
(*           (String.split models ~on:' ') *)
(*       in *)
(*       aux path (acc@new_acc) t *)
(*     | _ -> acc *)

(*   and list_cases path name (rmin, rmax) lmodels = *)
(*     List.range rmin (rmax+1) *)
(*     |> List.map ~f:(fun i->Printf.sprintf "%s_%d" name i) *)
(*     |> List.cartesian_product lmodels *)
(*     |> List.map ~f:(function (x,y)->build_case path y x ) *)

(*   and build_case path name model = { *)
(*     name = name ; *)
(*     result = Printf.sprintf "%s/%s.%s" path name model |> read_all *)
(*              |> String.strip |> float_of_string ; *)
(*     tree = Printf.sprintf "%s/%s.tree" path name |> TopoTree.of_newick_file ; *)
(*     seq = Printf.sprintf "%s/%s.seq" path name |> Align.of_fasta ; *)
(*   } *)

(*   in *)
(*   Printf.sprintf "%s/%s" path file *)
(*   |> Core_kernel.Std.In_channel.read_lines *)
(*   |> List.filter ~f:(function "" -> false | s -> not (s.[0]='#')) *)
(*   |> aux path [] *)

(* let of_testfile file f desc = *)
(*   read_test_file "test_data" file *)
(*   |> List.map ~f:( *)
(*     fun {name; result; tree; seq} -> *)
(*       Printf.sprintf "felsenstein_%s (%s vs bio++)" name desc, `Quick, *)
(*       (fun () -> f () tree seq |> check_likelihood result) *)
(*   ) *)

(* let test_K80_multi () = *)
(*   check_likelihood *)
(*     (Bpp_interface.felsenstein_bpp *)
(*        ~model:"\"K80(kappa=2.0)\"" *)
(*        ~path:"test_data" *)
(*        ~tree:"multi_1.tree" *)
(*        "multi_1.seq") *)
(*     Felsenstein.K80Felsenstein.(multi_felsenstein_shift () 2.0 *)
(*                                   (TopoTree.of_newick_file "test_data/multi_1.tree") *)
(*                                   (Align.of_fasta "test_data/multi_1.seq") *)
(*                                ) *)

(* let tests = [ *)
(*   "multi_shift K80", `Quick, test_K80_multi ; *)
(* ] @ of_testfile "test_single_small" (felsenstein ~site:0 ()) "normal" *)
(*   @ of_testfile "test_single_small" (felsenstein_shift ~site:0 ()) "shift" *)
(*   @ of_testfile "test_single_small" (felsenstein_logshift ~site:0 ()) "log shift" *)
(*   @ of_testfile "test_single_big" (felsenstein_shift ~site:0 ()) "shift" *)
(*   @ of_testfile "test_single_big" (felsenstein_logshift ~site:0 ()) "log shift" *)
(*   @ of_testfile "test_multi" (multi_felsenstein ()) "normal" *)
(*   @ of_testfile "test_multi" (multi_felsenstein_shift ()) "shift" *)
(*   @ of_testfile "test_multi" (multi_felsenstein_logshift ()) "log shift" *)

