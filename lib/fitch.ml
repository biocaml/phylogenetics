open Core_kernel

let default_cost x y =
  if x = y then 0. else 1.

let rec forward ?(cost = default_cost) ~n ~category (t : (_,'l,_) Tree.t) =
  match t with
  | Leaf l ->
    let costs =
      match category l with
      | Some cat ->
        if cat < 0 || cat >= n then invalid_arg "category returned integer not in [0;n[" ;
        Array.init n ~f:(fun i -> if i = cat then 0. else Float.infinity)
      | None -> Array.create ~len:n 0.
    in
    costs, Tree.leaf l
  | Node node ->
    let children_costs, children =
      List1.map node.branches ~f:(fun (Branch b) ->
          let cost, child = forward ~cost ~n ~category b.tip in
          cost, Tree.branch b.data child
        )
      |> List1.unzip
    in
    let costs, choices =
      Array.init n ~f:(fun i ->
          let costs_for_root_i =
            List1.map children_costs ~f:(fun costs ->
                let cost j = costs.(j) +. cost i j in
                let rec loop j best_cost best_choice =
                  if j = n then (best_cost, best_choice)
                  else
                    let candidate_cost = cost j in
                    if Float.(candidate_cost < best_cost) then
                      loop (j + 1) candidate_cost j
                    else
                      loop (j + 1) best_cost best_choice
                in
                loop 1 (cost 0) 0
              )
          in
          let costs, choices = List1.unzip costs_for_root_i in
          let total_cost = List1.fold costs ~init:0. ~f:( +. ) in
          total_cost, choices
        )
      |> Array.unzip
    in
    costs, Tree.node (node.data, choices) children

let rec backward_aux t i = match t with
  | Tree.Leaf l -> Tree.leaf (l, i)
  | Node n ->
    Tree.node (fst n.data, i) (
      List1.map2_exn n.branches (snd n.data).(i) ~f:(fun (Branch b) choice ->
          Tree.branch b.data (backward_aux b.tip choice)
        )
    )

let array_min_elt_index xs ~compare =
  match xs with
  | [| |] -> None
  | _ ->
    let n = Array.length xs in
    let rec loop acc i =
      if i >= n then Some acc
      else
        let acc' =
          match compare xs.(acc) xs.(i) with
          | -1
          |  0 -> acc
          |  1 -> i
          |  _ -> assert false
        in
        loop acc' (i + 1)
    in
    loop 0 1

let backward costs t =
  match array_min_elt_index costs ~compare:Float.compare with
  | None -> assert false
  | Some root -> backward_aux t root

let fitch ?cost ~n ~category t =
  let costs, routing = forward ?cost ~n ~category t in
  backward costs routing

let%expect_test "fitch" =
  let node x y = Tree.node () List1.(cons (Tree.branch () x) [ Tree.branch () y ]) in
  let leaf x = Tree.leaf x in
  let t = node (node (leaf 0) (leaf 1)) (leaf 0) in
  let p (_, i) = Int.to_string i in
  fitch ~category:Option.return ~n:2 t
  |> Tree.to_printbox ~leaf:p ~node:p
  |> PrintBox_text.output stdout ;
  [%expect {|
    0
    `+- 0
     |  `+- 0
     |   +- 1
     +- 0 |}]

let%expect_test "fitch_2" =
  let node x y = Tree.node () List1.(cons (Tree.branch () x) [ Tree.branch () y ]) in
  let leaf x = Tree.leaf x in
  let p (_, i) = Int.to_string i in
  let t = node (node (leaf 0) (leaf 1)) (node (leaf 1) (leaf 2)) in
  fitch ~n:3 ~category:Option.return t
  |> Tree.to_printbox ~leaf:p ~node:p
  |> PrintBox_text.output stdout ;
  [%expect {|
    1
    `+- 1
     |  `+- 0
     |   +- 1
     +- 1
        `+- 1
         +- 2 |}]
