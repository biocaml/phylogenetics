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

let propagate t ~init ~node ~leaf ~branch =
  let rec inner acc t =
    match t with
    | Node n ->
      let acc = node acc n.data in
      let branches = Non_empty_list.map n.branches ~f:(inner_branch acc) in
      Node { data = acc ; branches }
    | Leaf l -> Leaf (leaf acc l)

  and inner_branch acc (Branch b) =
    Branch {
      data = b.data ;
      tip = inner (branch acc b.data) b.tip
    }
  in
  inner init t

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


let rec leafset_generated_subtree t f leaves =
  match t with
  | Leaf l ->
    Option.bind (f l) ~f:(fun id ->
        if List.mem leaves id ~equal:String.equal then Some t
        else None
      )
  | Node n ->
    Non_empty_list.filter_map n.branches ~f:(fun (Branch b) ->
        leafset_generated_subtree b.tip f leaves
        |> Option.map ~f:(branch b.data)
      )
    |> Option.map ~f:(node n.data)

let%test "leafset_generated_subtree" =
  let node x y = binary_node () (branch () x) (branch () y) in
  let leaf x = leaf (Some x) in
  let t =
    node
      (node
         (node (leaf "A") (leaf "B"))
         (node (leaf "C") (leaf "D")))
      (leaf "E")
  in
  leafset_generated_subtree t Fn.id [] = None
  && leafset_generated_subtree t Fn.id ["A";"B";"C";"D";"E"] = Some t
