(* Type for evolutionary trees with nothing at the internal
   nodes, single bases at the leaves and length on edges (floats)*)
type base = A | T | G | C
and branch = float * tree
and tree =
  | Node of branch * branch
  | Leaf of base
;;

(* Pretty printing functions for our tree type *)
let string_of_base = function
  | A -> "A"
  | T -> "T"
  | G -> "G"
  | C -> "C"
;;

let print_base base = print_string (string_of_base base);;

let indent n =
  let f x = ' ' in
  String.init (4*n) f
;;

let string_of_tree tree =
  let rec aux tree level =
    match tree with
    | Leaf (base) ->
      Printf.sprintf "Base: %s\n" (string_of_base base)
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

(* Some code to test all of the above *)
let mytree = Node (
    (3.14, Node (
        (1.23, Leaf T ),
        (2.3, Node (
            (2.1, Leaf G),
            (5.2, Leaf A)
          )
        )
      )
    ),
    (2.3, Node (
        (5.2, Leaf G),
        (5.3, Leaf G)
      )
    )
  );;

let printline () = print_string "==========================\n";;
printline ();;
print_string (string_of_tree mytree);;
printline ();;
Printf.printf "%F %F\n" (jcModel A A) (jcModel A G);;
printline ();;
Printf.printf "%F %F\n" (probaChange jcModel A T 2.1) (probaChange jcModel A G 2.1);;
printline ();;
