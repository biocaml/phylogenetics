open Core_kernel.Std

module Make (E:Sigs.EVOL_MODEL) = struct
  include Seqgen.Make (E)

  let generate_trees ~(sampler:unit->TopoTree.t) amount =
    List.init amount ~f:(fun _ -> sampler ())

  let reject p align trees =
    List.filter_map trees ~f:(fun t -> if Align.equal align (seqgen p t 1) then Some t else None)

  let mean_floats l =
    List.fold l ~init:0.0 ~f:(fun acc x -> acc +. x)
    /. float_of_int (List.length l)

  let get_branch i trees =
    List.map trees ~f:(fun t -> List.nth_exn (TopoTree.get_branch_lengths t) i)

  let mean_specific_branch i trees =
    get_branch i trees |> mean_floats

end

module RI = Make(Models.K80)

let myalign = RI.Align.of_string_list ["A";"A";"A";"T"]

let mybasetree = TopoTree.of_preorder "0.1;0.1;0.1;0.1;0;1;0.1;0.1;2;3"

let mysampler = TopoTree.sample_branch_lengths ~branchs:(fun i -> i=5) ~sampler:(TopoTree.sample_float_uniform 1.0) mybasetree

let test () =
  let prior_trees = RI.generate_trees ~sampler:mysampler 1000000 in
  let post_trees = RI.reject 2.0 myalign prior_trees in
  Printf.printf "Prior branch lengths: %f\nPosterior branch lengths: %f\n(Size = %d/%d)"
    (RI.mean_specific_branch 5 prior_trees)
    (RI.mean_specific_branch 5 post_trees)
    (List.length post_trees)
    (List.length prior_trees) ;
  RI.get_branch 5 prior_trees |> List.map ~f:string_of_float |> Out_channel.write_lines "prior.txt" ;
  RI.get_branch 5 post_trees |> List.map ~f:string_of_float |> Out_channel.write_lines "post.txt" ;

