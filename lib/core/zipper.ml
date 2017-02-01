open Printf
open Core_kernel.Std
open Phylogenetic_tree


(* ======= *)
(*  TYPES  *)
(* ======= *)
type branch = float * Phylogenetic_tree.t
and metadata = {routing:routing_table; me:int}
and t =
  | ZipNode of {b0:branch; b1:branch; b2:branch; meta:metadata}
  | ZipBranch of {b0:branch; b1:branch; meta:metadata}
  | ZipLeaf of {index:Sigs.index; b0:branch; meta:metadata}
and routing_move =
  | RoutingStay
  | RoutingLeft of int
  | RoutingRight of int
  | RoutingNeighbour of direction
and routing_table = (int * routing_move) list
and direction = Dir0 | Dir1 | Dir2

type location_type = LocLeaf | LocBranch | LocNode
type oriented_zipper = {dir:direction; zipper:t}


(* ======================== *)
(*  DIRECTIONS / LOCATIONS  *)
(* ======================== *)
let string_of_dir = function Dir0 -> "Dir0" | Dir1 -> "Dir1" | Dir2 -> "Dir2"

let dir_of_string = function "Dir0" -> Dir0 | "Dir1" -> Dir1 | "Dir2" -> Dir2 | _ -> failwith "Unexpected direction name."

let location = function ZipNode _ -> LocNode | ZipLeaf _ -> LocLeaf | ZipBranch _ -> LocBranch

let left z = match location z.zipper, z.dir with
  | LocNode, Dir0 -> Dir1
  | LocBranch, _ | LocNode, Dir1 | LocNode, Dir2 -> Dir0
  | LocLeaf, _ -> failwith "Zipper already at leaf!"

let right z = match location z.zipper, z.dir with
  | LocBranch, _ | LocNode, Dir2 -> Dir1
  | LocNode, Dir0 | LocNode, Dir1 -> Dir2
  | LocLeaf, _ -> failwith "Zipper already at leaf!"


(* ============== *)
(*  CONSTRUCTORS  *)
(* ============== *)
let routing_set (t:routing_table) ~index:i ~move:m = List.Assoc.add t i m
let routing_get (t:routing_table) ~index:i = List.Assoc.find_exn t i

let build_leaf ?(old_routing=[]) ?(me= -1) index (l,t0) =
  let routing = (* point only neighbour to current pos *)
    routing_set ~index:(get_routing_no t0) ~move:(RoutingNeighbour Dir0) old_routing
    |> routing_set ~index:me ~move:RoutingStay (* point new pos to itself *)
  in ZipLeaf {index; b0=(l,t0); meta={routing; me}}

let build_branch ?(old_routing=[]) (l0,t0) (l1,t1) =
  let routing = (* point neighbours to current pos *)
    routing_set ~index:(get_routing_no t0) ~move:(RoutingNeighbour Dir0) old_routing
    |> routing_set ~index:(get_routing_no t1) ~move:(RoutingNeighbour Dir1)
    |> routing_set ~index:0 ~move:RoutingStay (* point new pos to itself *)
  in ZipBranch {b0=(l0,t0); b1=(l1,t1); meta={routing; me=0}} (* branch index is always 0 *)

let build_node ?(old_routing=[]) ?(me= -1) (l0,t0) (l1,t1) (l2,t2) =
  let routing = (* point neighbours to current pos *)
    routing_set ~index:(get_routing_no t0) ~move:(RoutingNeighbour Dir0) old_routing
    |> routing_set ~index:(get_routing_no t1) ~move:(RoutingNeighbour Dir1)
    |> routing_set ~index:(get_routing_no t2) ~move:(RoutingNeighbour Dir2)
    |> routing_set ~index:me ~move:RoutingStay (* point new pos to itself *)
  in ZipNode {b0=(l0,t0); b1=(l1,t1); b2=(l2,t2); meta={routing; me}}

(* let set_meta z m = match z with *)
(*   | ZipLeaf {index; b0; _} -> ZipLeaf {index; b0; meta=m} *)
(*   | ZipBranch {b0; b1; _} -> ZipBranch {b0; b1; meta=m} *)
(*   | ZipNode {b0; b1; b2; _} -> ZipNode {b0; b1; b2; meta=m} *)

let get_meta = function
  | ZipLeaf {meta; _} | ZipBranch {meta; _} | ZipNode {meta; _} -> meta


(* ========== *)
(*  MOVEMENT  *)
(* ========== *)
let slide z d l = match d, z with
  | Dir0, ZipLeaf {index=i; b0=l2, t; meta={routing; me}} when l<l2
    -> build_branch ~old_routing:routing (l2-.l, t) (l, Phylogenetic_tree.build_leaf ~routing_no:me i)
  | Dir0, ZipBranch {b0=l1,t1; b1=l2,t2; meta={routing; _}} when l<l1
    -> build_branch ~old_routing:routing (l1-.l,t1) (l2+.l,t2)
  | Dir1, ZipBranch {b0=l1,t1; b1=l2,t2; meta={routing; _}} when l<l2
    -> build_branch ~old_routing:routing (l1+.l,t1) (l2-.l,t2)
  | (Dir0, ZipNode {b0=lf,tf; b1=bb1; b2=bb2; meta={routing; me}} |
     Dir1, ZipNode {b1=lf,tf; b0=bb1; b2=bb2; meta={routing; me}} |
     Dir2, ZipNode {b2=lf,tf; b0=bb1; b1=bb2; meta={routing; me}}) when l<lf
    -> build_branch ~old_routing:routing (lf-.l,tf) (l,Phylogenetic_tree.build_node ~routing_no:me bb1 bb2)
  | Dir1, ZipLeaf _ | Dir2, ZipLeaf _ | Dir2, ZipBranch _
    -> failwith "Incorrect direction/zipper type combination (eg, move Dir1 on a leaf)."
  | _ -> failwith "Cannot slide: length too long."

let move z i = match i, z with
  (* case 1: moving from leaf to node *)
  | Dir0, ZipLeaf {index=i; b0=l,Node {left=b0; right=b1; meta={routing_no; _}}; meta={routing; me}}
    -> build_node ~old_routing:routing ~me:routing_no b0 b1 (l,Phylogenetic_tree.build_leaf ~routing_no:me i)

  (* case 2: moving from leaf to leaf (degenerate case) *)
  | Dir0, ZipLeaf {index=i; b0=l,Leaf {index=j; meta={routing_no; _}}; meta={routing; me}}
    -> build_leaf ~old_routing:routing ~me:routing_no j (l, Phylogenetic_tree.build_leaf ~routing_no:me i)

  (* case 3 moving from branch to node *)
  | Dir0, ZipBranch {b0=l0, Node {left=x; right=y; meta={routing_no; _}}; b1=l1, z; meta={routing; _}} |
    Dir1, ZipBranch {b1=l0, Node {left=x; right=y; meta={routing_no; _}}; b0=l1, z; meta={routing; _}}
    -> build_node ~old_routing:routing ~me:routing_no x y (l0+.l1,z)

  (* case 4: moving from branch to leaf *)
  | Dir0, ZipBranch {b0=l0, Leaf {index=i; meta={routing_no; _}}; b1=l1, z; meta={routing; _}} |
    Dir1, ZipBranch {b1=l0, Leaf {index=i; meta={routing_no; _}}; b0=l1, z; meta={routing; _}}
    -> build_leaf ~old_routing:routing ~me:routing_no i (l1+.l0, z)

  (* case 5: moving from node to node *)
  | Dir0, ZipNode {b0=l,Node {left=x; right=y; meta={routing_no; _}}; b1=(la,ta); b2=(lb,tb); meta={routing; me}} |
    Dir1, ZipNode {b1=l,Node {left=x; right=y; meta={routing_no; _}}; b0=(la,ta); b2=(lb,tb); meta={routing; me}} |
    Dir2, ZipNode {b2=l,Node {left=x; right=y; meta={routing_no; _}}; b0=(la,ta); b1=(lb,tb); meta={routing; me}}
    -> let new_routing = (* changing the routing of old neighbours to old position; relies on order on directions (left<right) *)
         routing_set ~index:(get_routing_no ta) ~move:(RoutingLeft me) routing
         |> routing_set ~index:(get_routing_no tb) ~move:(RoutingRight me)
    in build_node ~old_routing:new_routing ~me:routing_no x y (l, Phylogenetic_tree.build_node ~routing_no:me (la,ta) (lb,tb))

  (* case 6: move from node to leaf *)
  | Dir0, ZipNode {b0=l,Leaf {index=i; meta={routing_no; _}}; b1=(la,ta); b2=(lb,tb); meta={routing; me}} |
    Dir1, ZipNode {b1=l,Leaf {index=i; meta={routing_no; _}}; b0=(la,ta); b2=(lb,tb); meta={routing; me}} |
    Dir2, ZipNode {b2=l,Leaf {index=i; meta={routing_no; _}}; b0=(la,ta); b1=(lb,tb); meta={routing; me}}
    -> let new_routing = (* changing the routing of old neighbours to old position; relies on order on directions (left<right) *)
         routing_set ~index:(get_routing_no ta) ~move:(RoutingLeft me) routing
         |> routing_set ~index:(get_routing_no tb) ~move:(RoutingRight me)
    in build_leaf ~old_routing:new_routing ~me:routing_no i (l, Phylogenetic_tree.build_node ~routing_no:me (la,ta) (lb,tb))

  (* case 7: incorrect direction  given the type of zipper *)
  | Dir1, ZipLeaf _ | Dir2, ZipLeaf _ | Dir2, ZipBranch _
    -> failwith "Incorrect direction/zipper type combination (eg, move Dir1 on a leaf)."

let move_left z =
  (* relies on the fact that moving to an ZipNode always comes from Dir2 *)
  {dir=Dir2; zipper=move z.zipper (left z)}

let move_right z =
  (* relies on the fact that moving to an ZipNode always comes from Dir2 *)
  {dir=Dir2; zipper=move z.zipper (right z)}

let orient (z:t) d = {dir=d; zipper=z}


(* ================ *)
(*  ROUTING TABLES  *)
(* ================ *)
let rec init_routing =
  let rec compute_routing_tree table m =
    let routing_add (t, i) m = routing_set ~index:i ~move:m t, i+1 in
    function
    | Node {left=fl,l; right=fr,r; meta={id; _}} ->
      let (_,me) = table in
      let table2 = routing_add table m in (* add yourself to the table *)
      let table3, new_l = compute_routing_tree table2 (RoutingLeft me) l in
      let table4, new_r = compute_routing_tree table3 (RoutingRight me) r in
      table4, Node {left=fl,new_l; right=fr,new_r; meta={id; routing_no=me}}
    | Leaf {index; meta={id; _}} -> (* add leaf to table *)
      let (_,me) = table in
      routing_add table m,
      Leaf {index; meta={id; routing_no=me}}
  in
  function
  | ZipLeaf {index; b0=l0,t0; _} -> (* index 0 is reserved for branches and index 1 for the current position *)
    let (table,_), new_tree = compute_routing_tree ([],2) (RoutingNeighbour Dir0) t0 in
    ZipLeaf {index; b0=l0,new_tree; meta={routing=(routing_set table ~index:1 ~move:RoutingStay); me=1}}
  | ZipBranch _ as z-> init_routing (move z Dir0)
  | ZipNode {b0=l0,t0; b1=l1,t1; b2=l2,t2; _} ->
    let table, new_t0 = compute_routing_tree ([],2) (RoutingNeighbour Dir0) t0 in
    let table2, new_t1 = compute_routing_tree table (RoutingNeighbour Dir1) t1 in
    let (table3,_), new_t2 = compute_routing_tree table2 (RoutingNeighbour Dir2) t2 in
    ZipNode {b0=l0,new_t0; b1=l1,new_t1; b2=l2,new_t2; meta={routing=(routing_set table3 ~index:1 ~move:RoutingStay); me=1}}

let get_route table i =
  let rec aux table i =
    match routing_get table ~index:i with
    | (RoutingLeft j | RoutingRight j) as m -> m::(aux table j)
    | RoutingNeighbour _ as m -> [m]
    | RoutingStay as m -> [m]
  in List.rev (aux table i)

let goto z i =
  let rec follow z = function
    | (RoutingLeft _)::route -> follow (move_left z) route
    | (RoutingRight _)::route -> follow (move_right z) route
    | [] -> z
    | (RoutingNeighbour _ | RoutingStay)::_ -> failwith "Encountered unexpected move in route."
  in
  match get_route ((get_meta z).routing) i with
  | (RoutingNeighbour d)::route -> (follow (orient (move z d) Dir2) route).zipper
  | [RoutingStay] -> z
  | r -> failwith (sprintf "Empty or malformed route (len=%d)." (List.length r))


(* =================== *)
(*  GETTERS / SETTERS  *)
(* =================== *)
let get_length z d = match d, z with
  | Dir0, ZipLeaf {b0=l,_; _} |
    Dir0, ZipBranch {b0=l,_;_} |
    Dir1, ZipBranch {b1=l,_;_} |
    Dir0, ZipNode {b0=l,_;_} |
    Dir1, ZipNode {b1=l,_;_} |
    Dir2, ZipNode {b2=l,_;_} -> l
  | Dir1, ZipLeaf _ | Dir2, ZipLeaf _ | Dir2, ZipBranch _ ->
    failwith "Incorrect direction/zipper type combination (eg, move Dir1 on a leaf)."

let length_left z =
  left z |> get_length z.zipper

let length_right z =
  right z |> get_length z.zipper

let get_index = function
  | ZipLeaf {index; _} -> index
  | ZipBranch _ | ZipNode _ -> failwith "Zipper is not a leaf. Cannot get index."


(* ======================= *)
(*  CREATION / CONVERSION  *)
(* ======================= *)
let of_tree = function
  | Node {left=b0; right=b1; _} -> build_branch b0 b1
  | Leaf _ -> failwith "Zipper cannot be a lone leaf."

let of_tree_dir t =
  orient (of_tree t) Dir0 (* Dir0 is arbitrary here *)

let rec to_tree = function
  | (ZipNode {b0=l,_; _} |
     ZipLeaf {b0=l,_; _}) as z -> slide z Dir0 (l/.2.) |> to_tree
  | ZipBranch {b0; b1; _} -> Phylogenetic_tree.build_node b0 b1

let branch z d = match d, z with
  | Dir0, ZipLeaf {b0=x;_} |
    Dir0, ZipBranch {b0=x;_} |
    Dir1, ZipBranch {b1=x;_} |
    Dir0, ZipNode {b0=x;_} |
    Dir1, ZipNode {b1=x;_} |
    Dir2, ZipNode {b2=x;_} -> x
  | Dir1, ZipLeaf _ | Dir2, ZipLeaf _ | Dir2, ZipBranch _ ->
    failwith "Incorrect direction/zipper type combination (eg, move Dir1 on a leaf)."


(* ============ *)
(*  COMPARISON  *)
(* ============ *)
let equal z1 z2 = match z1, z2 with
  | ZipLeaf {index=i; b0=b,t; _}, ZipLeaf {index=i2; b0=b2,t2; _} ->
    Phylogenetic_tree.(i=i2 && (get_meta t).id = (get_meta t2).id && b=b2)
  | ZipBranch {b0=b00,t00; b1=b01,t01; _}, ZipBranch {b0=b10,t10; b1=b11,t11; _} ->
    Phylogenetic_tree.(
      (get_meta t00).id = (get_meta t10).id && b00 = b10 &&
      (get_meta t01).id = (get_meta t11).id && b01 = b11
    )
  | ZipNode {b0=b00,t00; b1=b01,t01; b2=b02,t02; _}, ZipNode {b0=b10,t10; b1=b11,t11; b2=b12,t12; _} ->
    Phylogenetic_tree.(
      (get_meta t00).id = (get_meta t10).id && b00 = b10 &&
      (get_meta t01).id = (get_meta t11).id && b01 = b11 &&
      (get_meta t02).id = (get_meta t12).id && b02 = b12
    )
  | ZipLeaf _, ZipBranch _ | ZipLeaf _, ZipNode _ |
    ZipBranch _, ZipLeaf _ | ZipBranch _, ZipNode _ |
    ZipNode _, ZipLeaf _ | ZipNode _, ZipBranch _ -> false


(* ================= *)
(*  PRETTY PRINTING  *)
(* ================= *)
let to_pretty_string =
  let indent sep str =
    String.rstrip str |>
    String.concat_map ~f:(fun x -> if x='\n' then sprintf "\n%s" sep else String.init 1 ~f:(fun _ ->x))
    |> sprintf "%s%s\n" sep in

  let string_of_branch z d =
    match branch z d with l,t ->
      Phylogenetic_tree.pp Format.str_formatter t ;
      Format.flush_str_formatter () |> indent "|    " |>
      sprintf "* Branch %s length=%.3f [%d]\n|\n%s|" (string_of_dir d) l (Phylogenetic_tree.get_meta t).id

  in function
    | ZipLeaf {index; meta={me;_}; _} as z ->
      sprintf "\n<ZipLeaf %s, routing_no=%d>\n%s" index me
        (string_of_branch z Dir0 |> indent ". ")
    | ZipBranch {meta={me; _}; _} as z ->
      sprintf "\n<ZipBranch routing_no=%d>\n%s%s" me
        (string_of_branch z Dir0 |> indent ". ")
        (string_of_branch z Dir1 |> indent ". ")
    | ZipNode {meta={me; _}; _} as z ->
      sprintf "\n<ZipNode routing_no=%d>\n%s%s%s" me
        (string_of_branch z Dir0 |> indent ". ")
        (string_of_branch z Dir1 |> indent ". ")
        (string_of_branch z Dir2 |> indent ". ")

let pp = Utils.pp to_pretty_string

let print = Utils.print ~options:[Utils.dim "/\\|-<>"] to_pretty_string
