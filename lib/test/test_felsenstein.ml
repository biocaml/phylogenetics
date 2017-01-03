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

let test_felsenstein_tiny () =
  felsenstein ~site:0 () () (TopoTree.of_preorder "0.1;0.1;0;1") (Align.of_string_list ["C";"G"]) |>
  check_likelihood (-4.22471668644312)




(* TODO finish functions below *)

let build_felsenstein (model: (module Sigs.EVOL_MODEL)) =
  let module M = (val model) in
  let module FG = Felsenstein.Felsenstein (M) in
  (module FG:Sigs.FELSENSTEIN)

let test_felsenstein
    ?(model=(module Models.JC69:Sigs.EVOL_MODEL))
    ?(treesize=5)
    ?(seqsize=5)
    comment
  =
  let module F = (val build_felsenstein model) in
  let tree = TopoTree.make_random treesize in
 ()





type test_case = {
  name: string ;
  result: float ;
  tree: TopoTree.t ;
  seq: Align.t ;
}

let read_test_file path file =
  let open Core_kernel.Std.In_channel in
  let rec aux path acc = function
    | name::range::models::t ->
      let new_acc = list_cases path name
          (Scanf.sscanf range "%d-%d" (fun x y->x,y))
          (String.split models ~on:' ')
      in
      aux path (acc@new_acc) t
    | _ -> acc

  and list_cases path name (rmin, rmax) lmodels =
    List.range rmin (rmax+1)
    |> List.map ~f:(fun i->Printf.sprintf "%s_%d" name i)
    |> List.cartesian_product lmodels
    |> List.map ~f:(function (x,y)->build_case path y x )

  and build_case path name model = {
    name = name ;
    result = Printf.sprintf "%s/%s.%s" path name model |> read_all
             |> String.strip |> float_of_string ;
    tree = Printf.sprintf "%s/%s.tree" path name |> TopoTree.of_newick_file ;
    seq = Printf.sprintf "%s/%s.seq" path name |> Align.of_fasta ;
  }

  in
  Printf.sprintf "%s/%s" path file
  |> Core_kernel.Std.In_channel.read_lines
  |> List.filter ~f:(function "" -> false | s -> not (s.[0]='#'))
  |> aux path []

let of_testfile file f desc =
  read_test_file "test_data" file
  |> List.map ~f:(
    fun {name; result; tree; seq} ->
      Printf.sprintf "felsenstein_%s (%s vs bio++)" name desc, `Quick,
      (fun () -> f () tree seq |> check_likelihood result)
  )

let test_K80_multi () =
  check_likelihood
    (Bpp_interface.felsenstein_bpp
       ~model:"\"K80(kappa=2.0)\""
       ~path:"test_data"
       ~tree:"multi_1.tree"
       "multi_1.seq")
    Felsenstein.K80Felsenstein.(multi_felsenstein_shift () 2.0
                                  (TopoTree.of_newick_file "test_data/multi_1.tree")
                                  (Align.of_fasta "test_data/multi_1.seq")
                               )

let tests = [
  "felsenstein_tiny", `Quick, test_felsenstein_tiny ;
  "multi_shift K80", `Quick, test_K80_multi ;
] @ of_testfile "test_single_small" (felsenstein ~site:0 ()) "normal"
  @ of_testfile "test_single_small" (felsenstein_shift ~site:0 ()) "shift"
  @ of_testfile "test_single_small" (felsenstein_logshift ~site:0 ()) "log shift"
  @ of_testfile "test_single_big" (felsenstein_shift ~site:0 ()) "shift"
  @ of_testfile "test_single_big" (felsenstein_logshift ~site:0 ()) "log shift"
  @ of_testfile "test_multi" (multi_felsenstein ()) "normal"
  @ of_testfile "test_multi" (multi_felsenstein_shift ()) "shift"
  @ of_testfile "test_multi" (multi_felsenstein_logshift ()) "log shift"

