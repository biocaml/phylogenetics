open Printf
open LATools
open TopoTree

(* ========================
   ||                    ||
   ||     SIGNATURES     ||
   ||                    ||
   ======================== *)

(* base models (UNUSED FOR NOW) *)
module type EVOL_BASE =
sig
  type base
  val string_of_base: base -> string
  val base_of_string: string -> base
  val int_of_base: base -> int
  val base_of_int: int -> base
  val print_base: base -> unit
  val nb_base:int
end


(* evolution models  *)
module type EVOL_MODEL =
sig
  include EVOL_BASE
  val transition: base -> base -> float
end


(* ========================
   ||                    ||
   ||    BASIC MODELS    ||
   ||                    ||
   ======================== *)

type dna = A | T | G | C

module DNA =
struct
  type base = dna

  let string_of_base = function A -> "A" | T -> "T" | G -> "G" | C -> "C"

  let base_of_string = function
    | "A" -> A
    | "C" -> C
    | "G" -> G
    | "T" -> T
    | _ -> invalid_arg "base_of_string"


  let int_of_base = function A -> 0 | C -> 1 | G -> 2 | T -> 3

  let nb_base = 4

  let base_of_int = function
    | 0 -> A
    | 1 -> C
    | 2 -> G
    | 3 -> T
    | x ->
      invalid_arg (sprintf "base_of_int: %d is not a correct dna base index" x)


  let print_base base = print_string (string_of_base base)
end

module JCModel =
struct
  include DNA

  let transition a b = if a=b then -3./.4. else 1./.4.
end


(* ========================
   ||                    ||
   ||      FUNCTORS      ||
   ||                    ||
   ======================== *)

module Felsenstein =
  functor (Mod: EVOL_MODEL) ->
  struct

    let transition_of_int x y =
      Mod.transition (Mod.base_of_int (x-1)) (Mod.base_of_int (y-1))


    let rate_matrix () = LATools.init 4 transition_of_int

    let eMt t = LATools.exp (LATools.scalmul (rate_matrix ()) t)

    (* let felsenstein tree sequences =  *)

    (* ========= *)
    (*   TESTS   *)
    (* ========= *)
    let test b1 b2 =
      let printline () = print_string "==========================\n" in
      printline ();
      Printf.printf "%F %F\n" (Mod.transition b1 b1) (Mod.transition b1 b2);
      printline () ;
      LATools.printMat (rate_matrix ()); Printf.printf "\n" ;
      printline () ;
      LATools.printMat (eMt 1.2) ;
      printline ()


  end


(* ========================
   ||                    ||
   ||       TESTS        ||
   ||                    ||
   ======================== *)

module JCFelsenstein = Felsenstein (JCModel)

(* let myvec = [|0.1;0.3;0.4;0.2|] *)
(* let initState = Vec.init 4 (function x -> myvec.(x-1)) *)
(* let testProd = gemv JCFelsenstein.rate_matrix initState *)

let test () =
  JCFelsenstein.test A T;
  match TopoTree.tree_of_string "1.2;1.3;5.2;2.3;3;5;2" with
  | Error e -> printf "Error: %s" e
  | Ok t -> TopoTree.pretty_print t
