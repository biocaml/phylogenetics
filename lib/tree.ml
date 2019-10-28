open Core_kernel

type ('a, 'b) t = {
  node_data : 'a ;
  branches : ('a, 'b) branch list ;
}

and ('a, 'b) branch = {
  branch_data : 'b ;
  tip : ('a, 'b) t ;
}

let node node_data branches = { node_data ; branches }
let branch branch_data tip = { branch_data ; tip }

let rec pre t ~init ~node ~branch =
  List.fold
    t.branches
    ~init:(node init t)
    ~f:(fun init -> pre_branch ~init ~node ~branch)

and pre_branch b ~init ~node ~branch =
  pre b.tip ~init:(branch init b) ~node ~branch

let map_leaves t ~root ~f =
  pre t ~init:([], root) ~branch:(fun (acc, _) b -> acc, b.branch_data) ~node:(fun (acc, b) t ->
      if t.branches = [] then (f t.node_data b :: acc, b)
      else (acc, b)
    )
  |> fst
  |> List.rev

let leaves t =
  pre t ~init:[] ~branch:(fun acc _ -> acc) ~node:(fun acc t ->
      if t.branches = [] then (t.node_data :: acc)
      else acc
    )
  |> List.rev

let rec map t ~node ~branch =
  { node_data = node t.node_data ;
    branches = List.map t.branches ~f:(map_branch ~node ~branch) }

and map_branch b ~node ~branch = {
  branch_data = branch b.branch_data ;
  tip = map b.tip ~node ~branch ;
}

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

let node_prefix_synthesis tree ~init ~f =
  let rec loop tree ~init =
    let state, children =
      List.fold_right tree.branches ~init:(init, []) ~f:(fun b (acc, t) ->
          let state, h = loop b.tip ~init:acc in
          state, h :: t
        )
    in
    let children_results = List.map children ~f:(fun n -> n.node_data) in
    let branches = List.map2_exn tree.branches children ~f:(fun b n -> { b with tip = n }) in
    let new_state, node_data = f state tree.node_data children_results in
    new_state, { node_data ; branches }
  in
  snd (loop tree ~init)
