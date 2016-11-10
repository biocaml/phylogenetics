type base = A | T | G | C ;;

type branch = float * tree
and tree =
  | Node of branch * branch
  | Leaf of base
;;

let pretty_print tree =
  let indent n string =
    let f x = ' ' in
    String.concat "" ((String.init (4*n) f)::[string])
  in
  let prindent n string = print_string (indent n string) in
  let prinbase = function
    | A -> print_string "A"
    | T -> print_string "T"
    | G -> print_string "G"
    | C-> print_string "C"
  in
  let rec aux tree level =
    match tree with
    | Leaf (base) ->
      print_string "BASE: " ;
      prinbase base;
      print_string "\n"
    | Node ((f1,b1),(f2,b2)) ->
      print_string "NODE\n" ;
      prindent level (string_of_float f1) ; print_string "->" ;
      aux b1 (level+1) ;
      prindent level (string_of_float f2) ; print_string "->" ;
      aux b2 (level+1)
  in
  aux tree 0
;;

let mytree = Node (
    (3.14, Node ((1.23, Leaf T ), (2.319, Leaf G)) ),
    (2.4, Leaf G )
  );;
pretty_print mytree;;
