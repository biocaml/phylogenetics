open Core_kernel.Std

let sample_float_uniform ?(min=0.0) max () = Random.float (max -. min) +. min

let sample_branch_lengths ~(branchs:int->bool) ~(sampler:unit->float) tree () =
  TopoTree.get_branch_lengths tree
  |> List.mapi ~f:(fun i l -> if branchs i then sampler () else l)
  |> TopoTree.set_branch_lengths tree

