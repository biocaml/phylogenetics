#require "lacaml";;
open Lacaml.S;;

(* base type *)
type base = A | T | G | C;;

let string_of_base = function
  | A -> "A"
  | T -> "T"
  | G -> "G"
  | C -> "C"
;;

let print_base base = print_string (string_of_base base);;

(* evolution models  *)
type model = base -> base -> float ;;

let jcModel a b = if a=b then -3./.4. else 1./.4. ;; (* not 100% sure about the -3/4 thing *)

(* let delta a b = if a=b then 1. else 0. ;; *)


(* ========= *)
(*   TESTS   *)
(* ========= *)
let printline () = print_string "==========================\n";;
printline ();;
Printf.printf "%F %F\n" (jcModel A A) (jcModel A G);;
printline ();;
