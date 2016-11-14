(* base type *)
type base = A | T | G | C;;

let string_of_base = function
  | A -> "A"
  | T -> "T"
  | G -> "G"
  | C -> "C"
;;

let print_base base = print_string (string_of_base base);;


(* Type for evolutionary trees: binary trees
   whose edges are labelled with lengths (floats)
   and whose leaves are labelled with sequence indexes (ints)*)
type index = int
and branch = float * tree
and tree =
  | Node of branch * branch
  | Leaf of index
;;

(* Generation of tree from dfs enumeration of nodes
   (some preliminary functions are required)*)
let pop_char s =
  match String.length s with
  | 0 -> None, ""
  | l -> Some s.[0], String.sub s (1) (l-1)
;;

let split_on_char sep s =
  let rec aux sep s buf acc =
    match pop_char s with
    | None, _ -> List.rev (buf::acc)
    | Some c, rest ->
      if c = sep then
        aux sep rest "" (buf::acc)
      else
        aux sep rest (String.concat "" [buf; String.make 1 c]) acc
  in aux sep s "" []
;;

type element = Int of int | Float of float;;
let element_of_string s =
  if String.contains s '.' then
    Float (float_of_string s)
  else
    Int (int_of_string s)
;;

let tree_of_string str =

  let rec init = function
    | [] -> None, [] (* empty token list *)
    | token :: rest ->
      match token with
      | Float f -> node f rest
      | Int i -> Some (Leaf i), rest

  and node f1 = function
    | (Float f2)::rest -> (
        match init rest with
        | Some tree, rest2 ->
        | _ -> None, []
      )
    | l -> None, l (* error expected second float *)

  in init (Int 1)
;;


(* Pretty printing functions for our tree type *)
let indent n =
  let f x = ' ' in
  String.init (4*n) f
;;

let string_of_tree tree =
  let rec aux tree level =
    match tree with
    | Leaf (index) ->
      Printf.sprintf "Base: %s\n" (string_of_int index)
    | Node ((f1,b1),(f2,b2)) ->
      Printf.sprintf "Node\n%s=(%F)=> %s%s=(%F)=> %s" (indent level) f1 (aux b1 (level+1)) (indent level) f2 (aux b2 (level+1))
  in
  aux tree 0
;;

(* evolution models  *)
type model = base -> base -> float ;;

let jcModel a b = if a=b then -3./.4. else 1./.4. ;; (* not 100% sure about the -3/4 thing *)

(* let delta a b = if a=b then 1. else 0. ;; *)

let probaChange m b1 b2 t = exp (-.(m b1 b2)*.t) ;;

let printline () = print_string "==========================\n";;
printline ();;
Printf.printf "%F %F\n" (jcModel A A) (jcModel A G);;
printline ();;
Printf.printf "%F %F\n" (probaChange jcModel A T 2.1) (probaChange jcModel A G 2.1);;
printline ();;
