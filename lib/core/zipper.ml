open Printf
open Core_kernel.Std
open TopoTree


(* ======= *)
(*  TYPES  *)
(* ======= *)
type branch = float * TopoTree.t
and zipper_metadata = {routing_nodes:routing_table; me:int}
and t =
  | ZipNode of {b0:branch; b1:branch; b2:branch; meta:zipper_metadata}
  | ZipBranch of {b0:branch; b1:branch; meta:zipper_metadata}
  | ZipLeaf of {index:Sigs.index; b0:branch; meta:zipper_metadata}
and routing_move =
  | RoutingStay
  | RoutingLeft of int
  | RoutingRight of int
  | RoutingNeighbour of direction
and routing_table = (int * routing_move) list
and direction = B0 | B1 | B2

type location_type = LocLeaf | LocBranch | LocNode
type oriented_zipper = {dir:direction; zipper:t}


(* ======================== *)
(*  DIRECTIONS / LOCATIONS  *)
(* ======================== *)
let string_of_dir = function B0 -> "B0" | B1 -> "B1" | B2 -> "B2"

let dir_of_string = function "B0" -> B0 | "B1" -> B1 | "B2" -> B2 | _ -> failwith "Unexpected direction name."

let location = function ZipNode _ -> LocNode | ZipLeaf _ -> LocLeaf | ZipBranch _ -> LocBranch

let left z = match location z.zipper, z.dir with
  | LocNode, B0 -> B1
  | LocBranch, _ | LocNode, B1 | LocNode, B2 -> B0
  | LocLeaf, _ -> failwith "Zipper already at leaf!"

let right z = match location z.zipper, z.dir with
  | LocBranch, _ | LocNode, B2 -> B1
  | LocNode, B0 | LocNode, B1 -> B2
  | LocLeaf, _ -> failwith "Zipper already at leaf!"


(* ============== *)
(*  CONSTRUCTORS  *)
(* ============== *)
let build_zleaf index b0 = ZipLeaf {index; b0; meta={routing_nodes=[]; me=0}}

let build_zbranch b0 b1 = ZipBranch {b0; b1; meta={routing_nodes=[]; me=0}}

let build_znode b0 b1 b2 = ZipNode {b0; b1; b2; meta={routing_nodes=[]; me=0}}

let set_meta z m = match z with
  | ZipLeaf {index; b0; _} -> ZipLeaf {index; b0; meta=m}
  | ZipBranch {b0; b1; _} -> ZipBranch {b0; b1; meta=m}
  | ZipNode {b0; b1; b2; _} -> ZipNode {b0; b1; b2; meta=m}

let get_meta = function
  | ZipLeaf {meta; _} | ZipBranch {meta; _} | ZipNode {meta; _} -> meta


(* ========== *)
(*  MOVEMENT  *)
(* ========== *)
let slide z d l = match d, z with
  | B0, ZipLeaf {index=i; b0=l2, t; _} when l<l2
    -> build_zbranch (l2-.l, t) (l, build_leaf i)
  | B0, ZipBranch {b0=l1,t1; b1=l2,t2; _} when l<l1 -> build_zbranch (l1-.l,t1) (l2+.l,t2)
  | B1, ZipBranch {b0=l1,t1; b1=l2,t2; _} when l<l2 -> build_zbranch (l1+.l,t1) (l2-.l,t2)
  | (B0, ZipNode {b0=lf,tf; b1=bb1; b2=bb2; _} |
     B1, ZipNode {b1=lf,tf; b0=bb1; b2=bb2; _} |
     B2, ZipNode {b2=lf,tf; b0=bb1; b1=bb2; _}) when l<lf
    -> build_zbranch (lf-.l,tf) (l,build_node_branch bb1 bb2)
  | B1, ZipLeaf _ | B2, ZipLeaf _ | B2, ZipBranch _
    -> failwith "Incorrect direction/zipper type combination (eg, move B1 on a leaf)."
  | _ -> failwith "Cannot slide: length too long."

let move z i = match i, z with
  (* case 1: zipper is at a leaf *)
  | B0, ZipLeaf {index=i; b0=l,Node {left=b0; right=b1; _}; _}
    -> build_znode b0 b1 (l,build_leaf i) (* moving to internal node *)
  | B0, ZipLeaf {index=i; b0=l,Leaf {index=j; _}; _}
    -> build_zleaf j (l, build_leaf i) (* moving to leaf (degenerate case)*)

  (* case 2: zipper is in the middle of a branch *)
  | B0, ZipBranch {b0=l0, Node {left=x; right=y; _}; b1=l1, z; _} | (* moving to internal node *)
    B1, ZipBranch {b1=l0, Node {left=x; right=y; _}; b0=l1, z; _}
    -> build_znode x y (l0+.l1,z)
  | B0, ZipBranch {b0=l0, Leaf {index=i; _}; b1=l1, z; _} | (* moving to leaf *)
    B1, ZipBranch {b1=l0, Leaf {index=i; _}; b0=l1, z; _}
    -> build_zleaf i (l1+.l0, z)

  (* case 3: zipper is at internal node *)
  | B0, ZipNode {b0=l,Node {left=x; right=y; _}; b1=a; b2=b; _} | (* moving to internal node*)
    B1, ZipNode {b1=l,Node {left=x; right=y; _}; b0=a; b2=b; _} |
    B2, ZipNode {b2=l,Node {left=x; right=y; _}; b0=a; b1=b; _}
    -> build_znode x y (l,build_node_branch a b)
  | B0, ZipNode {b0=l,Leaf {index=i; _}; b1=a; b2=b; _} | (* moving to leaf *)
    B1, ZipNode {b1=l,Leaf {index=i; _}; b0=a; b2=b; _} |
    B2, ZipNode {b2=l,Leaf {index=i; _}; b0=a; b1=b; _}
    -> build_zleaf i (l, build_node_branch a b)

  | B1, ZipLeaf _ | B2, ZipLeaf _ | B2, ZipBranch _
    -> failwith "Incorrect direction/zipper type combination (eg, move B1 on a leaf)."

let move_left z =
  (* relies on the fact that moving to an ZipNode always comes from B2 *)
  {dir=B2; zipper=move z.zipper (left z)}

let move_right z =
  (* relies on the fact that moving to an ZipNode always comes from B2 *)
  {dir=B2; zipper=move z.zipper (right z)}

let orient (z:t) d = {dir=d; zipper=z}


(* ================ *)
(*  ROUTING TABLES  *)
(* ================ *)
let routing_set (t:routing_table) i m = List.Assoc.add t i m
let routing_get (t:routing_table) i = List.Assoc.find_exn t i

let compute_routing =
  let rec compute_routing_tree tnodes m =
    let routing_add (t, i) m = routing_set t i m, i+1 in
    function
    | Node {left=_,l; right=_,r; _} ->
      let (_,me) = tnodes in
      let tnodes2 = routing_add tnodes m in (* add yourself to the table *)
      let tnodes3 = compute_routing_tree tnodes2 (RoutingLeft me) l in
      compute_routing_tree tnodes3 (RoutingRight me) r
    | Leaf _ -> (* add branch and leaf to tables *)
      routing_add tnodes m
  in
  function
  | ZipLeaf {b0=_,t0; _} ->
    let (tnodes,_) = compute_routing_tree ([],1) (RoutingNeighbour B0) t0 in tnodes
  | ZipBranch _ -> failwith "Routing from branch unsupported." (* TODO should it be supported again now that there is only node routing?*)
  | ZipNode {b0=_,t0; b1=_,t1; b2=_,t2; _} ->
    let tnodes = compute_routing_tree ([],1) (RoutingNeighbour B0) t0 in
    let tnodes2 = compute_routing_tree tnodes (RoutingNeighbour B1) t1 in
    let (tnodes3,_) = compute_routing_tree tnodes2 (RoutingNeighbour B2) t2 in
    tnodes3

let rec init_routing z =
  if location z = LocBranch then
    init_routing (move z B0)
  else
    let routing_nodes = compute_routing z in
    set_meta z {routing_nodes=(routing_set routing_nodes 0 RoutingStay); me=0}

let get_route_node table i =
  let rec aux table i =
    match routing_get table i with
    | (RoutingLeft j | RoutingRight j) as m -> m::(aux table j)
    | RoutingNeighbour _ as m -> [m]
    | RoutingStay as m -> [m]
  in List.rev (aux table i)

let goto_node z i =
  let rec follow z = function
    | (RoutingLeft _)::route -> follow (move_left z) route
    | (RoutingRight _)::route -> follow (move_right z) route
    | [] -> z
    | (RoutingNeighbour _ | RoutingStay)::_ -> failwith "Encountered unexpected move in route."
  in
  match get_route_node ((get_meta z).routing_nodes) i with
  | (RoutingNeighbour d)::route -> (follow (orient (move z d) B2) route).zipper
  | [RoutingStay] -> z
  | _ -> failwith "Empty or malformed route."


(* =================== *)
(*  GETTERS / SETTERS  *)
(* =================== *)
let get_length z d = match d, z with
  | B0, ZipLeaf {b0=l,_; _} |
    B0, ZipBranch {b0=l,_;_} |
    B1, ZipBranch {b1=l,_;_} |
    B0, ZipNode {b0=l,_;_} |
    B1, ZipNode {b1=l,_;_} |
    B2, ZipNode {b2=l,_;_} -> l
  | B1, ZipLeaf _ | B2, ZipLeaf _ | B2, ZipBranch _ ->
    failwith "Incorrect direction/zipper type combination (eg, move B1 on a leaf)."

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
let zipper_of_tree = function
  | Node {left=b0; right=b1; _} -> build_zbranch b0 b1
  | Leaf _ -> failwith "Zipper cannot be a lone leaf."

let dzipper_of_tree t =
  orient (zipper_of_tree t) B0 (* B0 is arbitrary here *)

let rec tree_of_zipper = function
  | (ZipNode {b0=l,_; _} |
     ZipLeaf {b0=l,_; _}) as z -> slide z B0 (l/.2.) |> tree_of_zipper
  | ZipBranch {b0; b1; _} -> build_node_branch b0 b1

let branch z d = match d, z with
  | B0, ZipLeaf {b0=x;_} |
    B0, ZipBranch {b0=x;_} |
    B1, ZipBranch {b1=x;_} |
    B0, ZipNode {b0=x;_} |
    B1, ZipNode {b1=x;_} |
    B2, ZipNode {b2=x;_} -> x
  | B1, ZipLeaf _ | B2, ZipLeaf _ | B2, ZipBranch _ ->
    failwith "Incorrect direction/zipper type combination (eg, move B1 on a leaf)."


(* ================= *)
(*  PRETTY PRINTING  *)
(* ================= *)
let indent sep str =
  String.rstrip str |>
  String.concat_map ~f:(fun x -> if x='\n' then sprintf "\n%s" sep else String.init 1 ~f:(fun _ ->x))
  |> sprintf "%s%s\n" sep

let string_of_branch z d =
  match branch z d with l,t ->
    TopoTree.pp Format.str_formatter t ;
    Format.flush_str_formatter () |> indent "|    " |>
    sprintf "<<%s (%.3f)>>\n|\n%s|" (string_of_dir d) l

let pp fmt = function
  | ZipLeaf _ as z ->
    Format.fprintf fmt "\n[[[[ ZipLeaf ]]]]\n%s"
      (string_of_branch z B0 |> indent ". ")
  | ZipBranch _ as z ->
    Format.fprintf fmt "\n[[[[ ZipBranch ]]]]\n%s%s"
      (string_of_branch z B0 |> indent ". ")
      (string_of_branch z B1 |> indent ". ")
  | ZipNode _ as z ->
    Format.fprintf fmt "\n[[[[ ZipNode ]]]]\n%s%s%s"
      (string_of_branch z B0 |> indent ". ")
      (string_of_branch z B1 |> indent ". ")
      (string_of_branch z B2 |> indent ". ")

let print = pp Format.std_formatter
