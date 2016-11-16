(* #require "lacaml";; *)
open Lacaml.S;;

(* ========================
   ||                    ||
   ||     SIGNATURES     ||
   ||                    ||
   ======================== *)

(* base models (UNUSED FOR NOW) *)
module type BASE_MODEL =
sig
  type base
  val string_of_base: base -> string
  val print_base: base -> unit
end;;


(* evolution models  *)
module type EVOL_MODEL =
sig
  type base
  val string_of_base: base -> string
  val transition: base -> base -> float
end;;


(* ========================
   ||                    ||
   ||    BASIC MODELS    ||
   ||                    ||
   ======================== *)

module DNA =
struct
  type base = A | T | G | C

  let string_of_base = function
    | A -> "A"
    | T -> "T"
    | G -> "G"
    | C -> "C"

  let print_base base = print_string (string_of_base base)
end;;

module JCModel =
struct
  include DNA
  let transition a b = if a=b then -3./.4. else 1./.4.
end;;


(* ========================
   ||                    ||
   ||      FUNCTORS      ||
   ||                    ||
   ======================== *)

module Felsenstein =
  functor (Mod: EVOL_MODEL) ->
  struct

    let rate_matrix = Mat.random 4 4 ;;

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


(* ========================
   ||                    ||
   ||       TESTS        ||
   ||                    ||
   ======================== *)

module JCFelsenstein = Felsenstein (JCModel);;

let test () =
  JCFelsenstein.test DNA.A DNA.T;;
