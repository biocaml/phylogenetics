open Printf
open Core_kernel.Std
open TopoTree


(* ======= *)
(*  TYPES  *)
(* ======= *)
type branch = float * TopoTree.t
type t =
  | InNode of {b0:branch; b1:branch; b2:branch}
  | MidBranch of {b0:branch; b1:branch}
  | Leaf of Sigs.index * branch

type direction = B0 | B1 | B2
type location_type = LocLeaf | LocBranch | LocNode

type oriented_zipper = direction * t


(* ======================== *)
(*  DIRECTIONS / LOCATIONS  *)
(* ======================== *)
let string_of_dir = function B0 -> "B0" | B1 -> "B1" | B2 -> "B2"

let dir_of_string = function "B0" -> B0 | "B1" -> B1 | "B2" -> B2 | _ -> failwith "Unexpected direction name."

let location = function InNode _ -> LocNode | Leaf _ -> LocLeaf | MidBranch _ -> LocBranch


(* ========== *)
(*  MOVEMENT  *)
(* ========== *)
let slide z d l = match d, z with
  | B0, Leaf (i,(l2, t)) when l<l2 -> MidBranch {b0=(l2-.l, t) ; b1=(l, TopoTree.Leaf i)}
  | B0, MidBranch {b0=l1,t1; b1=l2,t2} when l<l1 -> MidBranch {b0=l1-.l,t1; b1=l2+.l,t2}
  | B1, MidBranch {b0=l1,t1; b1=l2,t2} when l<l2 -> MidBranch {b0=l1+.l,t1; b1=l2-.l,t2}
  | (B0, InNode {b0=lf,tf; b1=bb1; b2=bb2} |
     B1, InNode {b1=lf,tf; b0=bb1; b2=bb2} |
     B2, InNode {b2=lf,tf; b0=bb1; b1=bb2}) when l<lf ->
    MidBranch {b0=lf-.l,tf; b1=l,Node (bb1,bb2) }
  | _ -> failwith "Cannot slide: length too long or incorrect direction"


let move z i = match i, z with
  | B0, Leaf (i,(l,Node (b0,b1))) -> InNode {b0;b1;b2=l,Leaf i}
  | B0, MidBranch {b0=l0, Node(x,y); b1=l1, z} |
    B1, MidBranch {b1=l0, Node(x,y); b0=l1, z} -> InNode {b0=x; b1=y; b2=l0+.l1,z}
  | B0, InNode {b0=l,Node (x,y); b1=a; b2=b} |
    B1, InNode {b1=l,Node (x,y); b0=a; b2=b} |
    B2, InNode {b2=l,Node (x,y); b0=a; b1=b} -> InNode {b0=x; b1=y; b2=l,Node(a,b)}
  | B0, Leaf (i,(l,Leaf j)) -> Leaf (j,(l,Leaf i))
  | B0, MidBranch {b0=l0,Leaf i; b1=l1, z} |
    B1, MidBranch {b1=l0,Leaf i; b0=l1, z} -> Leaf (i,(l1+.l0, z))
  | B0, InNode {b0=l,Leaf i; b1=a; b2=b} |
    B1, InNode {b1=l,Leaf i; b0=a; b2=b} |
    B2, InNode {b2=l,Leaf i; b0=a; b1=b} -> Leaf (i,(l, Node(a,b)))
  | _ -> failwith "Incorrect direction/zipper type combination (eg, move B1 on a leaf)."

let left (dr,z) =
  if location z = LocLeaf then
    failwith "Zipper already at a leaf!"
  else if location z = LocBranch then
    B2, move z B0
  else
    let move_to = match dr with
      | B1 | B2 -> B0
      | B0 -> B1
    in B2, move z move_to

let right (dr,z) =
  if location z = LocLeaf then
    failwith "Zipper already at a leaf!"
  else if location z = LocBranch then
    B2, move z B1
  else
    let move_to = match dr with
      | B0 | B1 -> B2
      | B2 -> B1
    in B2, move z move_to


(* ======================= *)
(*  CREATION / CONVERSION  *)
(* ======================= *)
let zipper_of_tree = function
  | Node (b0,b1) -> MidBranch {b0; b1}
  | _ -> failwith "Zipper cannot be a lone leaf."

let rec tree_of_zipper = function
  | (InNode {b0=l,_; _} |
     Leaf (_,(l,_))) as z -> slide z B0 (l/.2.) |> tree_of_zipper
  | MidBranch {b0;b1} -> Node (b0,b1)

let branch z d = match d, z with
  | _, Leaf (_,x) ->
    if d=B0 then x
    else failwith (sprintf "Leaf does not have a %s branch." (string_of_dir d))
  | B0, MidBranch {b0=x;_} |
    B1, MidBranch {b1=x;_} -> x
  | B2, MidBranch _ -> failwith "Midbranch does not have direction B2."
  | B0, InNode {b0=x;_} |
    B1, InNode {b1=x;_} |
    B2, InNode {b2=x;_} -> x

let orient z d = d, z


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
  | Leaf _ as z ->
    Format.fprintf fmt "\n[[[[ Leaf ]]]]\n%s"
      (string_of_branch z B0 |> indent ". ")
  | MidBranch _ as z ->
    Format.fprintf fmt "\n[[[[ MidBranch ]]]]\n%s%s"
      (string_of_branch z B0 |> indent ". ")
      (string_of_branch z B1 |> indent ". ")
  | InNode _ as z ->
    Format.fprintf fmt "\n[[[[ InNode ]]]]\n%s%s%s"
      (string_of_branch z B0 |> indent ". ")
      (string_of_branch z B1 |> indent ". ")
      (string_of_branch z B2 |> indent ". ")

let print = pp Format.std_formatter
