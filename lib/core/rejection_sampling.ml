open Core_kernel.Std

module Make (E:Sigs.EVOL_MODEL) = struct
  include Sequence_generation.Make (E)

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
