(* #require "lacaml";; *)
(* open Lacaml.S;; *)



(* evolution models  *)
module type EVOL_MODEL =
sig
  type base
  val transition: base -> base -> float
end;;


module Felsenstein =
  functor (Mod: EVOL_MODEL) ->
  struct
    (* ========= *)
    (*   TESTS   *)
    (* ========= *)
    let test b1 b2 =
      let printline () = print_string "==========================\n" in
      printline ();
      Printf.printf "%F %F\n" (Mod.transition b1 b1) (Mod.transition b1 b2);
      printline ()
    ;;

  end;;

type atgc = A | T | G | C ;;

module JCModel =
struct
  (* base type *)
  type base = atgc;;

  let string_of_base = function
    | A -> "A"
    | T -> "T"
    | G -> "G"
    | C -> "C"
  ;;

  let print_base base = print_string (string_of_base base);;

  let transition a b = if a=b then -3./.4. else 1./.4. ;;
end;;

module JCFelsenstein = Felsenstein (JCModel);;

let test () =
  JCFelsenstein.test A T
;;
