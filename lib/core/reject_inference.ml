open Core_kernel.Std

module Make (E:Sigs.EVOL_MODEL) = struct
  include Seqgen.Make (E)

  let generate_trees treesize size =
    List.init size ~f:(fun _ -> TopoTree.make_random treesize)

  let generate_aligns p trees =
    List.map trees ~f:(fun t -> t, seqgen p t 1)

  let reject align trees =
    List.filter_map trees ~f:(fun (t,a) -> if Align.equal align a then Some t else None)

  let mean_branch_length trees =
    List.fold trees ~init:0.0 ~f:(fun acc t -> acc +. (TopoTree.mean_branch_length t))
    /. float_of_int (List.length trees)

end

module JC69ri = Make(Models.K80)

let myalign = JC69ri.Align.of_string_list ["A";"A";"A";"A"]

let test () =
  let prior_trees = JC69ri.generate_trees (JC69ri.Align.nb_seq myalign) 1000000 in
  let post_trees = JC69ri.generate_aligns 2.0 prior_trees |> JC69ri.reject myalign in
  Printf.printf "Prior branch lengths: %f\nPosterior branch lengths: %f\n(Size = %d/%d)"
    (JC69ri.mean_branch_length prior_trees)
    (JC69ri.mean_branch_length post_trees)
    (List.length post_trees)
    (List.length prior_trees)
