open Core_kernel

module Cost = struct
  type t = Int of int | Infinity
  let zero = Int 0
  let succ = function
    | Int i -> Int (succ i)
    | Infinity -> Infinity

  let compare x y =
    match x, y with
    | Infinity, Infinity -> 0
    | Infinity, _ -> 1
    | _, Infinity -> -1
    | Int i, Int j -> Int.compare i j
      
    
  let ( < ) x y = match x, y with
    | Infinity, _ -> false
    | _, Infinity -> true
    | Int i, Int j -> i < j

  let ( + ) x y =
    match x, y with
    | Infinity, _
    | _, Infinity -> Infinity
    | Int i, Int j -> Int (i + j)
end

let rec forward ~n (t : _ Tree.t) : Cost.t array * (int List1.t array, int, 'b) Tree.t =
  match t with
  | Leaf l ->
    let costs = Array.init n ~f:(fun i -> if i = l then Cost.zero else Cost.Infinity) in
    costs, Leaf l
  | Node node ->
    let children_costs, children =
      List1.map node.branches ~f:(fun (Branch b) ->
          let cost, child = forward ~n b.tip in
          cost, Tree.branch b.data child
        )
      |> List1.unzip
    in
    let costs, choices =
      Array.init n ~f:(fun i ->
          let costs_for_root_i =
            List1.map children_costs ~f:(fun costs ->
                let cost j =
                  if i = j then costs.(j) else Cost.succ costs.(j)
                in
                let rec loop j best_cost best_choice =
                  if j = n then (best_cost, best_choice)
                  else
                    let candidate_cost = cost j in
                    if Cost.(candidate_cost < best_cost) then
                      loop (j + 1) candidate_cost j
                    else
                      loop (j + 1) best_cost best_choice
                in
                loop 1 (cost 0) 0
              )
          in
          let costs, choices = List1.unzip costs_for_root_i in
          let total_cost = List1.fold costs ~init:Cost.zero ~f:(Cost.( + )) in
          total_cost, choices
        )
      |> Array.unzip
    in
    costs, Tree.node choices children

let rec backward_aux t i = match t with
  | Tree.Leaf j -> assert (i = j) ; Tree.leaf i
  | Node n ->
    Tree.node i (
      List1.map2_exn n.branches n.data.(i) ~f:(fun (Branch b) choice ->
          Tree.branch b.data (backward_aux b.tip choice)
        )
    )

let backward costs t =
  let root = Owl.Utils.Array.min_i ~cmp:Cost.compare costs in
  backward_aux t root

let fitch ~n t =
  let costs, routing = forward ~n t in
  backward costs routing

let%expect_test "fitch" =
  let node x y = Tree.node () List1.(cons (Tree.branch () x) [ Tree.branch () y ]) in
  let leaf x = Tree.leaf x in
  let t = node (node (leaf 0) (leaf 1)) (leaf 0) in
  fitch ~n:2 t
  |> Tree.to_printbox ~leaf:Int.to_string ~node:Int.to_string
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
  let t = node (node (leaf 0) (leaf 1)) (node (leaf 1) (leaf 2)) in
  fitch ~n:3 t
  |> Tree.to_printbox ~leaf:Int.to_string ~node:Int.to_string
  |> PrintBox_text.output stdout ;
  [%expect {|
    1
    `+- 1
     |  `+- 0
     |   +- 1
     +- 1
        `+- 1
         +- 2 |}]
