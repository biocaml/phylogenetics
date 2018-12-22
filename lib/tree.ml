open Core_kernel

type ('a, 'b) t = {
  node_data : 'a ;
  branches : ('a, 'b) branch list ;
}

and ('a, 'b) branch = {
  branch_data : 'b ;
  tip : ('a, 'b) t ;
}

let rec pre t ~init ~node ~branch =
  List.fold
    t.branches
    ~init:(node init t)
    ~f:(fun init -> pre_branch ~init ~node ~branch)

and pre_branch b ~init ~node ~branch =
  pre b.tip ~init:(branch init b) ~node ~branch

let leaves t =
  pre t ~init:[] ~branch:(fun acc _ -> acc) ~node:(fun acc t ->
      if t.branches = [] then t.node_data :: acc
      else acc
    )
  |> List.rev

let propagate t ~root ~node ~branch =
  let rec inner parent_value parent_branch_value t =
    let node_data = node parent_value parent_branch_value in
    let branches = List.map t.branches ~f:(inner_branch node_data) in
    { node_data ; branches }

  and inner_branch parent_value b =
    { branch_data = branch parent_value b.branch_data ;
      tip = inner parent_value b.branch_data b.tip }
  in
  let branches = List.map t.branches ~f:(inner_branch root) in
  { node_data = root ; branches }
  
