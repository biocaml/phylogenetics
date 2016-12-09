(* Type for evolutionary trees: binary trees
   whose edges are labelled with lengths (floats)
   and whose leaves are labelled with sequence indexes (ints)*)
type index = int
and branch = float * t
and t =
  | Node of branch * branch
  | Leaf of index

(* Generation of tree from post-order enumeration of nodes
   (some preliminary functions are required)*)
let pop_char s =
  match String.length s with
  | 0 -> None, ""
  | l -> Some s.[0], String.sub s (1) (l-1)

let split_on_char sep s =
  let rec aux sep s buf acc =
    match pop_char s with
    | None, _ -> List.rev (buf::acc)
    | Some c, rest when c=sep -> aux sep rest "" (buf::acc)
    | Some c, rest -> aux sep rest (String.concat "" [buf; String.make 1 c]) acc
  in aux sep s "" []

type element = Int of int | Float of float
let element_of_string s =
  if String.contains s '.' then
    Float (float_of_string s)
  else
    Int (int_of_string s)

let tree_of_string str =
  let rec fulltree = function
    | [] -> Error "Empty token list."
    | token :: rest ->
      match token with
      | Float f -> node f rest
      | Int i -> Ok (Leaf i, rest)

  and node f1 = function
    | (Float f2)::rest -> left f1 f2 rest
    | _ -> Error "Expected second float"

  and left f1 f2 list =
    match fulltree list with
    | Ok (tree1, rest) -> right f1 f2 tree1 rest
    | Error m -> Error (Printf.sprintf "Left returned unexpected result: <%s>" m)

  and right f1 f2 tree1 list =
    match fulltree list with
    | Ok (tree2, rest) -> Ok (Node ((f1, tree1), (f2, tree2)), rest)
    | Error m -> Error (Printf.sprintf "Right returned unexpected result: <%s>" m)

  in
  match fulltree (List.map element_of_string (split_on_char ';' str)) with
  | Ok (t, _) -> t
  | Error m -> invalid_arg m

(* Pretty printing functions for our tree type *)
let indent n =
  let f x = ' ' in
  String.init (4*n) f

let pretty_tree tree =
  let rec aux tree level =
    match tree with
    | Leaf (index) ->
      Printf.sprintf "Index: %s\n" (string_of_int index)
    | Node ((f1,b1),(f2,b2)) ->
      Printf.sprintf "Node\n%s=(%F)=> %s%s=(%F)=> %s" (indent level) f1 (aux b1 (level+1)) (indent level) f2 (aux b2 (level+1))
  in
  aux tree 0

let pp fmt tree = pretty_tree tree |> Format.fprintf fmt "%s"


(* ======== *)
(*   TEST   *)
(* ======== *)
let test () =
  let printline () = print_string "==========================\n" in
  let print_treeresult str = pretty_tree str |> print_string in

  printline ();
  let mytree = tree_of_string "3.;4.;1.;2.;11;5.;6.;12;13;7.;8.;16;9.;10.;14;15" in
  print_treeresult mytree;
  printline ();
  let mytree = tree_of_string "1.0;2.0;1;2.1;3.2;3" in (* this is incorrect on purpose *)
  print_treeresult mytree;
  printline ();
