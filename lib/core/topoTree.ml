open Core_kernel.Std
open Printf

let _ = Random.self_init ()


(* ======= *)
(*  TYPES  *)
(* ======= *)
type t =
  | Node of {meta:metadata; left:branch; right:branch}
  | Leaf of {meta:metadata; index:Sigs.index}
and branch = float * t
and metadata = {id:int}

(* ======================= *)
(*  CREATION / CONVERSION  *)
(* ======================= *)
let of_newick str =
  let rec aux = function
    | Newick.Node (l::r::[],_) -> Node {left=branch l; right=branch r; meta={id=0}}
    | _ -> invalid_arg "Non-binary or malformed newick tree."

  and branch = function
    | {Newick.id=Some s; Newick.length=Some l; _} ->
      l, Leaf {index=s; meta={id=0}}
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
      | Int i -> Ok (Leaf {index=sprintf "T%d" i; meta={id=0}}, rest)

  and node f1 = function
    | (Float f2)::rest -> left f1 f2 rest
    | _ -> Error "Expected second float"

  and left f1 f2 list =
    match fulltree list with
    | Ok (tree1, rest) -> right f1 f2 tree1 rest
    | Error m -> Error (sprintf "Left returned unexpected result: <%s>" m)

  and right f1 f2 tree1 list =
    match fulltree list with
    | Ok (tree2, rest) -> Ok (Node {left=f1, tree1; right=f2, tree2; meta={id=0}}, rest)
    | Error m -> Error (sprintf "Right returned unexpected result: <%s>" m)

  in
  match fulltree (List.map ~f:element_of_string (String.split ~on:';' str)) with
  | Ok (t, _) -> t
  | Error m -> invalid_arg m

let make_random n =
  let rec aux = function
    | [t] -> t
    | _::_ as l->
      pick_two l ~f:(fun a b -> Node {left=rand_branch a; right=rand_branch b; meta={id=0}})
      |> aux
    | [] -> failwith "tree list should not be empty"
  and pick_two l ~f = match List.permute l with
    | a::b::tl -> (f a b)::tl
    | _ -> failwith "tried to pick_two in too short a list"
  and rand_branch t = ((Random.float 0.5)+.0.00001, t)
  in aux (List.init n ~f:(fun i -> (Leaf {index=sprintf "T%d" i; meta={id=0}})))

let to_newick t =
  let rec aux = function
    | Node {left=f1,l; right=f2,r; _} ->
      sprintf "(%s:%f,%s:%f)" (aux l) f1 (aux r) f2
    | Leaf {index=i; _} -> i
  in aux t |> sprintf "%s;"

let to_newick_file t filename =
  Out_channel.write_all filename ~data:(to_newick t)

let to_dot t =
  let rec aux n = function
    | Node {left=_,l; right=_,r; _} ->
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
  | Node {left=_,l; right=_,r; _} -> 2 + nb_branches l + nb_branches r
  | Leaf _ -> 0

let rec set_branch_lengths tree lengths = match tree, lengths with
  | (Leaf _ as l, []) -> l
  | (Node {left=_,l; right=_,r; meta}, l1::l2::tl) ->
    Node {
      left = l1, set_branch_lengths l (List.sub tl ~pos:0 ~len:(nb_branches l));
      right = l2, set_branch_lengths r (List.sub tl ~pos:(nb_branches l) ~len:(nb_branches r));
      meta
    }
  | _ -> failwith "Branch list does not match tree"

let rec get_branch_lengths = function
  | Node {left=l1,l; right=l2,r; _} -> l1::l2::(get_branch_lengths l)@(get_branch_lengths r)
  | Leaf _ -> []


(* ============== *)
(*  CONSTRUCTORS  *)
(* ============== *)
let get_id = function
  | Node {meta={id}; _} |
    Leaf {meta={id}; _} -> id

(* let build_id = function *)
(*   | Node {left=_,l; right=_,r; _} -> Hashtbl.hash (get_id l + get_id r) *)
(*   | Leaf {index; _} -> Hashtbl.hash index *)

let build_node f1 l f2 r =
  Node {left= f1,l; right=f2,r; meta={id=Hashtbl.hash (get_id l + get_id r)}}

let build_leaf i =
  Leaf {index=i; meta={id=Hashtbl.hash i}}


(* ================= *)
(*  PRETTY PRINTING  *)
(* ================= *)
type pp_aux = {
  text:string;
  size:int;
  stem:int
}

let pp2 tree =
  let indent sep str =
    String.rstrip str
    |> String.split_lines
    |> List.mapi ~f:(fun i line ->
        sprintf "%s%s" (sep i) line
      )
    |> String.concat ~sep:"\n"

  in let rec aux = function
      | Leaf {index=i; _} -> {
          text = sprintf "<%s>" i; size=1; stem=0
        }
      | Node {left=l1,l; right=l2,r; _} ->
        let aux1, aux2 = aux l, aux r in
        let l1_s, l2_s = sprintf "%.3f" l1, sprintf "%.3f" l2 in
        let l1_l, l2_l = String.length l1_s, String.length l2_s in
        let maxfl = max l1_l l2_l in
        let total, stem = maxfl+5, (aux1.stem + aux1.size + aux2.stem)/2 in
        let stemup = String.init total ~f:(function
            | 0 -> '/'
            | x when x>2 && x<(3+l1_l) -> l1_s.[x-3]
            | _ -> '-'
          ) in
        let stemdown = String.init total ~f:(function
            | 0 -> '\\'
            | x when x>2 && x<(3+l2_l) -> l2_s.[x-3]
            | _ -> '-'
          ) in
        let indentup i =
          if i<aux1.stem then String.make total ' '
          else if i=aux1.stem then stemup
          else String.init total ~f:(function 0->'|'|_->' ') in
        let indentdown i =
          if i>aux2.stem then String.make total ' '
          else if i=aux2.stem then stemdown
          else String.init total ~f:(function 0->'|'|_->' ')
        in

        {
          text = String.concat ~sep:"\n" [
              indent indentup aux1.text ;
              indent indentdown aux2.text
            ] ;
          size = aux1.size + aux2.size ;
          stem
        }
  in (aux tree).text

let pp fmt t = Format.fprintf fmt "%s\n" (pp2 t)
let print = pp Format.std_formatter

(* ========= *)
(*   TESTS   *)
(* ========= *)
let mytree = of_preorder "0.3;0.4;0.1;0.2;0;1;0.5;0.6;0.7;0.8;2;3;0.9;0.11;4;5"

let test () =
  pp2 mytree
