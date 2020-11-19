open Core_kernel

type ('n, 'l, 'b) t =
  | Node of {
      data : 'n ;
      branches : ('n, 'l, 'b) branch List1.t ;
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
  Node { data ; branches = List1.cons left [ right ] }

let branch data tip = Branch { data ; tip }

let data = function
  | Node n -> n.data
  | Leaf l -> l

module B = PrintBox

let rec to_printbox_aux t ?parent_branch ~node ~leaf ~branch = match t with
  | Leaf l -> B.text (leaf l)
  | Node n ->
    let node_text = match Option.bind parent_branch ~f:branch with
      | None -> node n.data
      | Some b_label -> sprintf "%s - %s" b_label (node n.data)
    in
    List1.map n.branches ~f:(fun (Branch b) ->
        to_printbox_aux ~parent_branch:b.data ~node ~leaf ~branch b.tip
      )
    |> List1.to_list
    |> B.tree (B.text node_text)

let to_printbox ?(node = fun _ -> "·") ?(leaf = fun _ -> "·") ?(branch = fun _ -> None) t =
  to_printbox_aux t ?parent_branch:None ~node ~leaf ~branch

let rec prefix_traversal t ~init ~node ~leaf ~branch =
  match t with
  | Leaf l -> leaf init l
  | Node n ->
    List1.fold
      n.branches
      ~init:(node init n.data)
      ~f:(fun init -> pre_branch ~init ~leaf ~node ~branch)

and pre_branch (Branch b) ~init ~node ~leaf ~branch =
  prefix_traversal b.tip ~init:(branch init b.data) ~leaf ~node ~branch

let fold_leaves t ~init ~f =
  prefix_traversal t ~init
    ~branch:(fun acc _ -> acc)
    ~node:(fun acc _ -> acc)
    ~leaf:(fun acc l -> f acc l)

let leaves t =
  prefix_traversal t ~init:[]
    ~branch:(fun acc _ -> acc)
    ~node:(fun acc _ -> acc)
    ~leaf:(fun acc l -> l :: acc)
  |> List.rev

let rec map t ~node ~leaf ~branch =
  match t with
  | Node n ->
    Node {
      data = node n.data ;
      branches = List1.map n.branches ~f:(map_branch ~node ~leaf ~branch) ;
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
      let branches = List1.map n.branches ~f:(inner_branch acc) in
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
    List1.filter_map n.branches ~f:(fun (Branch b) ->
        leafset_generated_subtree b.tip f leaves
        |> Option.map ~f:(branch b.data)
      )
    |> List1.of_list
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
  Poly.(leafset_generated_subtree t Fn.id [] = None)
  && Poly.(leafset_generated_subtree t Fn.id ["A";"B";"C";"D";"E"] = Some t)

let simplify_node_with_single_child ~merge_branch_data t =
  let rec prune_linear_root = function
    | Leaf _ as l -> l
    | Node n ->
      match n.branches with
      | Cons (Branch b, []) ->
        prune_linear_root b.tip
      | branches ->
        node n.data (List1.map branches ~f:traverse_branch)
  and traverse_branch (Branch b as bb) =
    match b.tip with
    | Leaf _ -> bb
    | Node n ->
      match n.branches with
      | Cons (Branch b', []) ->
        collapse_linear_segment ~branch_data:[b'.data ; b.data] b'.tip
      | branches ->
        let branches = List1.map branches ~f:traverse_branch in
        let tip = node n.data branches in
        branch b.data tip
  and collapse_linear_segment ~branch_data = function
    | Leaf _ as l ->
      branch (merge_branch_data branch_data) l
    | Node n ->
      match n.branches with
      | Cons (Branch b, []) ->
        collapse_linear_segment ~branch_data:(b.data :: branch_data) b.tip
      | branches ->
        let branches = List1.map branches ~f:traverse_branch in
        let tip = node n.data branches in
        branch (merge_branch_data branch_data) tip
  in
  prune_linear_root t

module Simplify_node_with_single_child_tests = struct
  let n1 x = node () List1.(cons (branch () x) [])
  let n2 x y = node () List1.(cons (branch () x) [ branch () y ])
  let leaf x = leaf x
  let print t =
    simplify_node_with_single_child t ~merge_branch_data:(fun _ -> ())
    |> to_printbox ~leaf:Fn.id
    |> PrintBox_text.output stdout

  let%expect_test "simplify_node_with_single_child" =
    let t =
      n2
        (n2
           (n1 (leaf "A"))
           (n2 (leaf "C") (leaf "D")))
        (leaf "E")
    in
    print t ;
    [%expect {|
    ·
    `+- ·
     |  `+- A
     |   +- ·
     |      `+- C
     |       +- D
     +- E |}]

  let%expect_test "simplify_node_with_single_child" =
    let t = n1 (n1 (leaf "E")) in
    print t ;
    [%expect {| E |}]

  let%expect_test _ =
    let t =
      n2
        (n1 (leaf "A"))
        (n1 (leaf "B"))
      |> n1
      |> n1
    in
    print t ;
    [%expect {|
      ·
      `+- A
       +- B |}]
end
