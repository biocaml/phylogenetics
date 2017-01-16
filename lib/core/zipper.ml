open Printf
open Core_kernel.Std
open TopoTree


(* ======= *)
(*  TYPES  *)
(* ======= *)
type branch = float * TopoTree.t
type t =
  | InNode of {b0:branch; b1:branch; b2:branch}

type direction = B0 | B1 | B2
type direction_oriented = Up | Left | Right

type oriented_zipper = direction * t


(* ======================= *)
(*  CREATION / CONVERSION  *)
(* ======================= *)
let zipper_of_tree = function
  | Node ( (l1,Node((l2,t1),(l3,t2))), (l4,t3)) |
    Node ((l4,t3), (l1,Node((l2,t1),(l3,t2))) ) ->
    InNode {b0=l1+.l4, t3; b1=l2, t1; b2=l3, t2}
  | _ -> failwith "Zipper cannot be positioned at a leaf."

let tree_of_zipper = function
  | InNode {b0=l1,t1; b1=l2,t2; b2=l3,t3} ->
    Node ( (l1/.2., t1), (l1/.2., Node ((l2, t2), (l3, t3))))

let branch z i = match i, z with
  | B0, InNode {b0=x;_} |
    B1, InNode {b1=x;_} |
    B2, InNode {b2=x;_} -> x

let orient z d = d, z


(* ========== *)
(*  MOVEMENT  *)
(* ========== *)
let move z i = match i, z with
  | B0, InNode {b0=l,Node (x,y); b1=a; b2=b} |
    B1, InNode {b1=l,Node (x,y); b0=a; b2=b} |
    B2, InNode {b2=l,Node (x,y); b0=a; b1=b} -> InNode {b0=x; b1=y; b2=l,Node(a,b)}
  | _ -> failwith "Cannot go in this direction because it is a leaf."

let rmove (dr,z) d =
  let move_to = match dr, d with
    | x, Up -> x
    | B1, Left | B2, Left -> B0
    | B0, Left | B2, Right -> B1
    | B0, Right | B1, Right -> B2
  in B2, move z move_to


(* ================= *)
(*  PRETTY PRINTING  *)
(* ================= *)
let indent sep str =
  String.rstrip str |>
  String.concat_map ~f:(fun x -> if x='\n' then sprintf "\n%s" sep else String.init 1 ~f:(fun _ ->x))
  |> sprintf "%s%s\n" sep

let string_of_dir = function B0 -> "B0" | B1 -> "B1" | B2 -> "B2"

let dir_of_string = function "B0" -> B0 | "B1" -> B1 | "B2" -> B2 | _ -> failwith "Unexpected direction name."

let string_of_branch z d =
  match branch z d with l,t ->
    TopoTree.pp Format.str_formatter t ;
    Format.flush_str_formatter () |> indent "|    " |>
    sprintf "<<%s (%F)>>\n|\n%s|" (string_of_dir d) l

let pp fmt (z:t) =
  Format.fprintf fmt "\n[[[[ZIPPER]]]]\n%s%s%s"
    (string_of_branch z B0 |> indent ". ")
    (string_of_branch z B1 |> indent ". ")
    (string_of_branch z B2 |> indent ". ")


(* ========= *)
(*  TESTING  *)
(* ========= *)
let print = pp Format.std_formatter

let test = zipper_of_tree (TopoTree.of_preorder "0.3;0.4;0.1;0.2;0;1;0.5;0.6;0.7;0.8;2;3;0.9;0.11;4;5")

let test2 = move test B0
