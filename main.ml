type base = A | T | G | C ;;

type branch = float * tree
and tree =
  | Node of branch * branch
  | Leaf of base
;;

let pretty_print tree =
  let indent n string =
    let f x = ' ' in
    String.concat "" ((String.init n f)::[string])
  in
  let rec aux tree level =
    match tree with
    | Leaf (base) ->
      print_string (indent level "BASE\n")
    | Node ((f1,b1),(f2,b2)) ->
      print_string (indent level "NODE\n") ; aux b1 (level+1) ; aux b2 (level+1)
  in
  aux tree 0
;;

let mytree = Node ( (1.0, Leaf (T) ), (2.0, Leaf (G) )  );;
pretty_print mytree;;
