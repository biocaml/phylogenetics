open Core_kernel
open Sigs

module Make(Align : ALIGNMENT) = struct
  let generate_trees ~(sampler:unit->Phylogenetic_tree.t) amount =
    List.init amount ~f:(fun _ -> sampler ())

  let reject seqgen p align trees =
    List.filter_map trees ~f:(fun t ->
        if Align.equal align (seqgen p t 1) then Some t else None
      )

  let mean_floats l =
    List.fold l ~init:0.0 ~f:(fun acc x -> acc +. x)
    /. float_of_int (List.length l)

  let get_branch i trees =
    List.map trees ~f:(fun t -> List.nth_exn (Phylogenetic_tree.get_branch_lengths t) i)

  let mean_specific_branch i trees =
    get_branch i trees |> mean_floats

end
