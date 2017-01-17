open Printf
open Core_kernel.Std
open TopoTree


(* ======= *)
(*  TYPES  *)
(* ======= *)
type branch = float * TopoTree.t
type t =
  | ZipNode of {b0:branch; b1:branch; b2:branch}
  | ZipBranch of {b0:branch; b1:branch}
  | ZipLeaf of Sigs.index * branch

type direction = B0 | B1 | B2
type location_type = LocZipLeaf | LocBranch | LocNode

type oriented_zipper = {dir:direction; zipper:t}


(* ======================== *)
(*  DIRECTIONS / LOCATIONS  *)
(* ======================== *)
let string_of_dir = function B0 -> "B0" | B1 -> "B1" | B2 -> "B2"

let dir_of_string = function "B0" -> B0 | "B1" -> B1 | "B2" -> B2 | _ -> failwith "Unexpected direction name."

let location = function ZipNode _ -> LocNode | ZipLeaf _ -> LocZipLeaf | ZipBranch _ -> LocBranch

let left z = match location z.zipper, z.dir with
  | LocNode, B0 -> B1
  | LocBranch, _ | LocNode, B1 | LocNode, B2 -> B0
  | LocZipLeaf, _ -> failwith "Zipper already at leaf!"

let right z = match location z.zipper, z.dir with
  | LocBranch, _ | LocNode, B2 -> B1
  | LocNode, B0 | LocNode, B1 -> B2
  | LocZipLeaf, _ -> failwith "Zipper already at leaf!"


(* ========== *)
(*  MOVEMENT  *)
(* ========== *)
let slide z d l = match d, z with
  | B0, ZipLeaf (i,(l2, t)) when l<l2 -> ZipBranch {b0=(l2-.l, t) ; b1=(l, Leaf i)}
  | B0, ZipBranch {b0=l1,t1; b1=l2,t2} when l<l1 -> ZipBranch {b0=l1-.l,t1; b1=l2+.l,t2}
  | B1, ZipBranch {b0=l1,t1; b1=l2,t2} when l<l2 -> ZipBranch {b0=l1+.l,t1; b1=l2-.l,t2}
  | (B0, ZipNode {b0=lf,tf; b1=bb1; b2=bb2} |
     B1, ZipNode {b1=lf,tf; b0=bb1; b2=bb2} |
     B2, ZipNode {b2=lf,tf; b0=bb1; b1=bb2}) when l<lf ->
    ZipBranch {b0=lf-.l,tf; b1=l,Node (bb1,bb2) }
  | B1, ZipLeaf _ | B2, ZipLeaf _ | B2, ZipBranch _ ->
    failwith "Incorrect direction/zipper type combination (eg, move B1 on a leaf)."
  | _ -> failwith "Cannot slide: length too long."

let move z i = match i, z with
  (* case 1: zipper is at a leaf *)
  | B0, ZipLeaf (i,(l,Node (b0,b1))) -> ZipNode {b0;b1;b2=l,Leaf i} (* moving to internal node *)
  | B0, ZipLeaf (i,(l,Leaf j)) -> ZipLeaf (j,(l,Leaf i)) (* moving to leaf (degenerate case)*)

  (* case 2: zipper is in the middle of a branch *)
  | B0, ZipBranch {b0=l0, Node(x,y); b1=l1, z} | (* moving to internal node *)
    B1, ZipBranch {b1=l0, Node(x,y); b0=l1, z} -> ZipNode {b0=x; b1=y; b2=l0+.l1,z}
  | B0, ZipBranch {b0=l0, Leaf i; b1=l1, z} | (* moving to leaf *)
    B1, ZipBranch {b1=l0, Leaf i; b0=l1, z} -> ZipLeaf (i,(l1+.l0, z))

  (* case 3: zipper is at internal node *)
  | B0, ZipNode {b0=l,Node (x,y); b1=a; b2=b} | (* moving to internal node*)
    B1, ZipNode {b1=l,Node (x,y); b0=a; b2=b} |
    B2, ZipNode {b2=l,Node (x,y); b0=a; b1=b} -> ZipNode {b0=x; b1=y; b2=l,Node(a,b)}
  | B0, ZipNode {b0=l,Leaf i; b1=a; b2=b} | (* moving to leaf *)
    B1, ZipNode {b1=l,Leaf i; b0=a; b2=b} |
    B2, ZipNode {b2=l,Leaf i; b0=a; b1=b} -> ZipLeaf (i,(l, Node(a,b)))

  | B1, ZipLeaf _ | B2, ZipLeaf _ | B2, ZipBranch _
    -> failwith "Incorrect direction/zipper type combination (eg, move B1 on a leaf)."

let move_left z =
  (* relies on the fact that moving to an ZipNode always comes from B2 *)
  {dir=B2; zipper=move z.zipper (left z)}

let move_right z =
  (* relies on the fact that moving to an ZipNode always comes from B2 *)
  {dir=B2; zipper=move z.zipper (right z)}


(* =================== *)
(*  GETTERS / SETTERS  *)
(* =================== *)
let get_length z d = match d, z with
  | B0, ZipLeaf (_,(l,_)) |
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
  | ZipLeaf (i,_) -> i
  | ZipBranch _ | ZipNode _ -> failwith "Zipper is not a leaf. Cannot get index."


(* ======================= *)
(*  CREATION / CONVERSION  *)
(* ======================= *)
let zipper_of_tree = function
  | Node (b0,b1) -> ZipBranch {b0; b1}
  | Leaf _ -> failwith "Zipper cannot be a lone leaf."

let dzipper_of_tree t =
  {dir=B0; zipper=zipper_of_tree t} (* B0 is arbitrary here *)

let rec tree_of_zipper = function
  | (ZipNode {b0=l,_; _} |
     ZipLeaf (_,(l,_))) as z -> slide z B0 (l/.2.) |> tree_of_zipper
  | ZipBranch {b0;b1} -> Node (b0,b1)

let branch z d = match d, z with
  | B0, ZipLeaf (_,x) |
    B0, ZipBranch {b0=x;_} |
    B1, ZipBranch {b1=x;_} |
    B0, ZipNode {b0=x;_} |
    B1, ZipNode {b1=x;_} |
    B2, ZipNode {b2=x;_} -> x
  | B1, ZipLeaf _ | B2, ZipLeaf _ | B2, ZipBranch _ ->
    failwith "Incorrect direction/zipper type combination (eg, move B1 on a leaf)."

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
