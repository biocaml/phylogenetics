open Core_kernel.Std
open Printf

let _ = Random.self_init ()


(* ======= *)
(*  TYPES  *)
(* ======= *)
type index = string
and branch = float * t
and t =
  | Node of branch * branch
  | Leaf of index

type zipper =
  | InNode of {b0:branch; b1:branch; b2:branch}


(* ================== *)
(*  ZIPPER FUNCTIONS  *)
(* ================== *)

let zipper_of_tree = function
  | Node ( (l1,Node((l2,t1),(l3,t2))), (l4,t3)) |
    Node ((l4,t3), (l1,Node((l2,t1),(l3,t2))) ) ->
    InNode {b0=l1+.l4, t3; b1=l2, t1; b2=l3, t2}
  | _ -> failwith "Zipper cannot be positioned at a leaf."

let tree_of_zipper = function
  | InNode {b0=l1,t1; b1=l2,t2; b2=l3,t3} ->
    Node ( (l1/.2., t1), (l1/.2., Node ((l2, t2), (l3, t3))))

let branch z i = match i, z with
  | 0, InNode {b0=x;_} |
    1, InNode {b1=x;_} |
    2, InNode {b2=x;_} -> x
  | i, _ -> failwith (sprintf "Invalid branch index %d. Index must be between 0 and 2." i)

let move z i = match i, z with
  | 0, InNode {b0=l,Node (x,y); b1=a; b2=b} |
    1, InNode {b1=l,Node (x,y); b0=a; b2=b} |
    2, InNode {b2=l,Node (x,y); b0=a; b1=b} -> InNode {b0=x; b1=y; b2=l,Node(a,b)}
  | i, _ when i>2 -> failwith (sprintf "Invalid branch index %d. Index must be between 0 and 2." i)
  | _ -> failwith "Cannot go in this direction because it is a leaf."


(* ======================= *)
(*  CREATION / CONVERSION  *)
(* ======================= *)
let of_newick str =
  let rec aux = function
    | Newick.Node (l::r::[],_) -> Node (branch l, branch r)
    | _ -> invalid_arg "Non-binary or malformed newick tree."

  and branch = function
    | {Newick.id=Some s; Newick.length=Some l; _} ->
      l, Leaf s
    | {Newick.length=Some l; Newick.tip=t; _} -> l, aux t
    | _ -> invalid_arg "Malformed branch in newick tree."

  in
  let mybuf = Lexing.from_string str in
  try
    Newick_parser.tree Newick_lexer.token mybuf |> aux
  with
  | Newick_parser.Error ->
    let open Lexing in
    let pos = mybuf.lex_curr_p in
    sprintf "Parser error (%s:%d:%d)" pos.pos_fname
      pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)
    |> failwith

let of_newick_file path =
  Core_kernel.Std.In_channel.read_all path
  |> of_newick

(* Generation of tree from post-order enumeration of nodes
   (some preliminary functions are required)*)
type element = Int of int | Float of float
let element_of_string s =
  if String.contains s '.' then
    Float (float_of_string s)
  else
    Int (int_of_string s)

let of_preorder str =
  let rec fulltree = function
    | [] -> Error "Empty token list."
    | token :: rest ->
      match token with
      | Float f -> node f rest
      | Int i -> Ok (Leaf (sprintf "T%d" i), rest)

  and node f1 = function
    | (Float f2)::rest -> left f1 f2 rest
    | _ -> Error "Expected second float"

  and left f1 f2 list =
    match fulltree list with
    | Ok (tree1, rest) -> right f1 f2 tree1 rest
    | Error m -> Error (sprintf "Left returned unexpected result: <%s>" m)

  and right f1 f2 tree1 list =
    match fulltree list with
    | Ok (tree2, rest) -> Ok (Node ((f1, tree1), (f2, tree2)), rest)
    | Error m -> Error (sprintf "Right returned unexpected result: <%s>" m)

  in
  match fulltree (List.map ~f:element_of_string (String.split ~on:';' str)) with
  | Ok (t, _) -> t
  | Error m -> invalid_arg m

let make_random n =
  let rec aux = function
    | [t] -> t
    | _::_ as l->
      pick_two l ~f:(fun a b -> Node (rand_branch a, rand_branch b))
      |> aux
    | [] -> failwith "tree list should not be empty"
  and pick_two l ~f = match List.permute l with
    | a::b::tl -> (f a b)::tl
    | _ -> failwith "tried to pick_two in too short a list"
  and rand_branch t = ((Random.float 0.5)+.0.00001, t)
  in aux (List.init n ~f:(fun i -> (Leaf (sprintf "T%d" i))))

let to_newick t =
  let rec aux = function
    | Node ((f1,l),(f2,r)) ->
      sprintf "(%s:%f,%s:%f)" (aux l) f1 (aux r) f2
    | Leaf i -> i
  in aux t |> sprintf "%s;"

let to_newick_file t filename =
  Out_channel.write_all filename ~data:(to_newick t)

let to_dot t =
  let rec aux n = function
    | Node ((_,l),(_,r)) ->
      (sprintf "\t%s -> %s_l;\n\t%s -> %s_r;\n" n n n n)
      :: (aux (sprintf "%s_l" n) l)
      @ (aux (sprintf "%s_r" n) r)
    | Leaf _ -> []
  in aux "root" t
     |> String.concat
     |> sprintf "digraph{\n%s}"

let index_of_string s = s

let index_of_int i = sprintf "T%d" i

(* ============================== *)
(*  PARAMETERS / TRANSFORMATIONS  *)
(* ============================== *)
let rec nb_branches = function
  | Node ((_,l),(_,r)) -> 2 + nb_branches l + nb_branches r
  | _ -> 0

let rec set_branch_lengths tree lengths = match tree, lengths with
  | (Leaf _ as l, []) -> l
  | (Node ((_,l),(_,r)), l1::l2::tl) ->
    Node (
      (l1, set_branch_lengths l (List.sub tl ~pos:0 ~len:(nb_branches l))),
      (l2, set_branch_lengths r (List.sub tl ~pos:(nb_branches l) ~len:(nb_branches r)))
    )
  | _ -> failwith "Branch list does not match tree"

let rec get_branch_lengths = function
  | Node ((l1,l),(l2,r)) -> l1::l2::(get_branch_lengths l)@(get_branch_lengths r)
  | _ -> []

let reroot t _ = t


(* ================= *)
(*  PRETTY PRINTING  *)
(* ================= *)
let pp fmt tree =
  let rec aux tree level =
    let indent n =
      let f _ = ' ' in
      String.init (4*n) ~f:f
    in
    match tree with
    | Leaf (index) ->
      sprintf "Index: %s\n" index
    | Node ((f1,b1),(f2,b2)) ->
      sprintf "Node\n%s=(%F)=> %s%s=(%F)=> %s"
        (indent level) f1 (aux b1 (level+1))
        (indent level) f2 (aux b2 (level+1))
  in
  aux tree 0 |> Format.fprintf fmt "%s"


(* ========= *)
(*   TESTS   *)
(* ========= *)
let mytree = of_preorder "0.1;0.1;0.1;0.1;0;1;0.1;0.1;2;0.1;0.1;3;4"

let test () =
  Out_channel.write_all "tmp.dot" ~data:(to_dot mytree)
