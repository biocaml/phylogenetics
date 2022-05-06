open Core

type sample_list = float list

let sample_branch_lengths ~(branchs:int->bool) ~(sampler:unit->float) tree () =
  Phylogenetic_tree.get_branch_lengths tree
  |> List.mapi ~f:(fun i l -> if branchs i then sampler () else l)
  |> Phylogenetic_tree.set_branch_lengths tree

let sample_list_of_file path =
  In_channel.read_lines path
  |> List.map ~f:(float_of_string)

let sample_list_extrema d =
  match
    List.min_elt ~compare:Float.compare d, List.max_elt ~compare:Float.compare d
  with
    (Some mi, Some ma) -> (mi, ma) | _ -> failwith "empty input distribution"

let sample_list_mean d =
  List.fold d ~init:(0., 0) ~f:(fun (s, c) x -> (s+.x, c+1))
  |> fun (s, c) -> s /. (float_of_int c)

