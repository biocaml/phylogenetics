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

let of_newick str =
  let aux = function
    | Newick.Node (_,_) -> Leaf 0

  in
  Newick_parser.tree Newick_lexer.token (Lexing.from_string str)
  |> aux

let of_newick_file path =
  Core_kernel.Std.In_channel.read_all path
  |> of_newick

let of_preorder str =
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

(* Pretty printing *)
let pp fmt tree =
  let rec aux tree level =
    let indent n =
      let f _ = ' ' in
      String.init (4*n) f
    in
    match tree with
    | Leaf (index) ->
      Printf.sprintf "Index: %s\n" (string_of_int index)
    | Node ((f1,b1),(f2,b2)) ->
      Printf.sprintf "Node\n%s=(%F)=> %s%s=(%F)=> %s"
        (indent level) f1 (aux b1 (level+1))
        (indent level) f2 (aux b2 (level+1))
  in
  aux tree 0 |> Format.fprintf fmt "%s"
