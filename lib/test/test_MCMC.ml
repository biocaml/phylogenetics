open Core_kernel.Std
open Alcotest
open Biocaml_phylogeny_core
open MCMC


(** {6 Test input parameters} *)

module MyMCMC = Make(Models.K80)

let my_likelihood v =
  Pervasives.exp (MyMCMC.felsenstein 2.0 v.tree v.align) *.
  (let newlength = List.nth_exn (Phylogenetic_tree.get_branch_lengths v.tree) 5 in
   if newlength>0. && newlength<5. then 1.0 else 0.0)

let myalign = MyMCMC.Align.of_string_list ["A"; "A"; "A"; "T"]
let mybasetree = Phylogenetic_tree.of_preorder "0.1;0.1;0.1;0.1;0;1;2.5;0.1;2;3"
let my_theta0 = {align=my_align; tree=my_basetree}


let my_step v =
  let lengths = Phylogenetic_tree.get_branch_lengths v.tree |> List.mapi ~f:(
      let range = 5.0 in
      fun i x -> if i=5
        then x -. (range/.2.) +. (Random.float range)
        else x
    ) in
  let new_tree = Phylogenetic_tree.set_branch_lengths v.tree lengths in
  {align=v.align; tree=new_tree}, 1.


(** {6 Test functions} *)

let sample amount = MyMCMC.run my_theta0 my_step my_likelihood amount
             |> List.map ~f:(function {tree;_} ->
                 List.nth_exn (Phylogenetic_tree.get_branch_lengths tree) 5)
             |> List.filteri ~f:(fun x _ -> x > amount / 5)

let test_MCMC () =
  Test_utils.check_distrib [2.8] (sample 10000)


(** {6 Test list} *)

let tests = [
  "Specific branch length on tiny tree with 500k points.", `Slow, test_MCMC]
