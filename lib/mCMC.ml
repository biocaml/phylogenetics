open Core_kernel

let accept p =
  Random.float 1.0 <= p

let run (theta0:'a) (step:'a->'a*float) (likelihood:'a->float) (nb_points:int) =
  let rec aux i prev acc =
    if i>=nb_points then acc
    else
      let candidate, hastings_ratio = step prev in
      let full_ratio = hastings_ratio *. (likelihood candidate) /. (likelihood prev) in
      (* FIXME unnecessary computation of f prev *)
      if accept full_ratio then aux (i+1) candidate (candidate::acc)
      else aux i prev (prev::acc)
  in
  aux 0 theta0 []

module M = struct
  module Alignment = Alignment.Make(Seq.DNA)
  module Felsenstein = Felsenstein.Make(Nucleotide)(Alignment)(Models.K80)
end

type vector = {
  tree : Phylogenetic_tree.t ;
  align : M.Alignment.t
}

let my_likelihood v =
  let open M in
  Stdlib.exp (Felsenstein.felsenstein 2.0 v.tree v.align) *.
  (let newlength = List.nth_exn (Phylogenetic_tree.get_branch_lengths v.tree) 5 in
   if newlength>0. && newlength<5. then 1.0 else 0.0)

let my_align = M.Alignment.of_string_list ["A";"A";"A";"T"]
let my_basetree = Phylogenetic_tree.of_preorder "0.1;0.1;0.1;0.1;0;1;3.0;0.1;2;3"
let my_theta0 = { align=my_align ; tree=my_basetree }

let my_step v =
  let lengths = Phylogenetic_tree.get_branch_lengths v.tree |> List.mapi ~f:(
      let range = 5.0 in
      fun i x -> if i=5
        then x -. (range/.2.) +. (Random.float range)
        else x
    ) in
  let new_tree = Phylogenetic_tree.set_branch_lengths v.tree lengths in
  {align=v.align; tree=new_tree}, 1.

let test i = run my_theta0 my_step my_likelihood i
             |> List.map ~f:(function {tree;_} ->
                 List.nth_exn (Phylogenetic_tree.get_branch_lengths tree) 5)
             |> List.filteri ~f:(fun x _ -> x>i/5)
             |> Stat_tools.plot_sample_list
           ; Stat_tools.pause ()
