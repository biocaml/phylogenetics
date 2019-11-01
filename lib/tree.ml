open Core_kernel

type ('n, 'l, 'b) t =
  | Node of {
      data : 'n ;
      branches : ('n, 'l, 'b) branch Non_empty_list.t ;
    }
  | Leaf of 'l

and ('n, 'l, 'b) branch = Branch of {
  data : 'b ;
  tip : ('n, 'l, 'b) t ;
}

let leaf l = Leaf l

let node data branches =
  Node { data ; branches }

let binary_node data left right =
  Node { data ; branches = Non_empty_list.cons left [ right ] }

let branch data tip = Branch { data ; tip }


let data = function
  | Node n -> n.data
  | Leaf l -> l

let rec pre t ~init ~node ~leaf ~branch =
  match t with
  | Leaf l -> leaf init l
  | Node n ->
    Non_empty_list.fold
      n.branches
      ~init:(node init n.data)
      ~f:(fun init -> pre_branch ~init ~leaf ~node ~branch)

and pre_branch (Branch b) ~init ~node ~leaf ~branch =
  pre b.tip ~init:(branch init b.data) ~leaf ~node ~branch

let map_leaves t ~root ~f =
  pre t ~init:([], root)
    ~branch:(fun (acc, _) b -> acc, b)
    ~node:(fun (acc, b) _ -> acc, b)
    ~leaf:(fun (acc, b) l -> (f l b :: acc, b))
  |> fst
  |> List.rev

let leaves t =
  pre t ~init:[]
    ~branch:(fun acc _ -> acc)
    ~node:(fun acc _ -> acc)
    ~leaf:(fun acc l -> l :: acc)
  |> List.rev

let rec map t ~node ~leaf ~branch =
  match t with
  | Node n ->
    Node {
      data = node n.data ;
      branches = Non_empty_list.map n.branches ~f:(map_branch ~node ~leaf ~branch) ;
    }
  | Leaf l -> Leaf (leaf l)

and map_branch (Branch b) ~node ~leaf ~branch =
  Branch {
    data = branch b.data ;
    tip = map b.tip ~node ~leaf ~branch ;
  }

let propagate t ~root:(parent_value, parent_branch_value) ~node ~leaf =
  let rec inner parent_value parent_branch_value t =
    match t with
    | Node n ->
      let data = node parent_value parent_branch_value in
      let branches = Non_empty_list.map n.branches ~f:(inner_branch data) in
      Node { data ; branches }
    | Leaf _ -> Leaf (leaf parent_value parent_branch_value)

  and inner_branch parent_value (Branch b) =
    Branch {
      data = b.data ;
      tip = inner parent_value b.data b.tip
    }
  in
  inner parent_value parent_branch_value t

(* let node_prefix_synthesis tree ~init ~f =
 *   let rec loop tree ~init =
 *     let state, children =
 *       List.fold_right tree.branches ~init:(init, []) ~f:(fun b (acc, t) ->
 *           let state, h = loop b.tip ~init:acc in
 *           state, h :: t
 *         )
 *     in
 *     let children_results = List.map children ~f:(fun n -> n.node_data) in
 *     let branches = List.map2_exn tree.branches children ~f:(fun b n -> { b with tip = n }) in
 *     let new_state, node_data = f state tree.node_data children_results in
 *     new_state, { node_data ; branches }
 *   in
 *   snd (loop tree ~init) *)
