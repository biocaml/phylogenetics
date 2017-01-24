open Core_kernel.Std

let accept p =
  Random.float 1.0 <= p

module Make (E:Sigs.EVOL_MODEL) = struct
  include Felsenstein.Make(E)
  include E

  let run (theta0:'a) (step:'a->'a*float) (likelihood:'a->float) (nb_points:int) =
    let rec aux i prev acc =
      if i>=nb_points then acc
      else
        let candidate, hastings_ratio = step prev in
        let full_ratio = hastings_ratio *. (likelihood candidate)/.(likelihood prev) in (* FIXME unnecessary computation of f prev *)
        if accept full_ratio then aux (i+1) candidate (candidate::acc)
        else aux i prev acc
    in
    aux 0 theta0 []

end

module MyMCMC = Make(Models.K80)

type vector = {tree:TopoTree.t; align:MyMCMC.Align.t}

let my_likelihood v = Pervasives.exp (MyMCMC.felsenstein 2.0 v.tree v.align) *.
                      (let newlength = List.nth_exn (TopoTree.get_branch_lengths v.tree) 5 in
                       if newlength>0. && newlength<5. then 1.0 else 0.0)

let my_align = MyMCMC.Align.of_string_list ["A";"A";"A";"T"]
let my_basetree = TopoTree.of_preorder "0.1;0.1;0.1;0.1;0;1;0.8;0.1;2;3"
let my_theta0 = {align=my_align; tree=my_basetree}

let my_step v =
  let lengths = TopoTree.get_branch_lengths v.tree |> List.mapi ~f:(fun i x -> if i=5 then x -. 0.125 +. (Random.float 0.25) else x) in
  let new_tree = TopoTree.set_branch_lengths v.tree lengths in
  {align=v.align; tree=new_tree}, 1.

let test i = MyMCMC.run my_theta0 my_step my_likelihood i
             |> List.map ~f:(function {tree;_} -> List.nth_exn (TopoTree.get_branch_lengths tree) 5)
             |> List.filteri ~f:(fun x _ -> x>i/5)
             |> StatTools.plot_sample_list
           ; StatTools.pause ()
