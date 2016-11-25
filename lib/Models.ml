open Printf
open DNA

(* ========================
   ||                    ||
   ||     SIGNATURES     ||
   ||                    ||
   ======================== *)

(* base models (UNUSED FOR NOW) *)
(* module type EVOL_BASE = *)
(* sig *)
(*   type base *)
(*   val string_of_base: base -> string *)
(*   val base_of_string: string -> base *)
(*   val int_of_base: base -> int *)
(*   val base_of_int: int -> base *)
(*   val print_base: base -> unit *)
(*   val nb_base:int *)
(* end *)


(* evolution models  *)
module type EVOL_MODEL =
sig
  (* include EVOL_BASE*)
  type base
  val base_of_int: int -> base
  val transition: base -> base -> float
end


(* ========================
   ||                    ||
   ||    BASIC MODELS    ||
   ||                    ||
   ======================== *)
module JCModel =
struct
  type base = dna
  let base_of_int = dna_of_int
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

    open TopoTree

    let transition_of_int x y =
      Mod.transition (Mod.base_of_int (x-1)) (Mod.base_of_int (y-1))

    let rate_matrix () = LATools.init 4 transition_of_int

    let eMt t = LATools.exp (LATools.scalmul (rate_matrix ()) t)

    let known_vector b =
      LATools.initvec 4 (fun x->if x=int_of_dna b then 1. else 0.)

    let felsenstein t sequences = match t with
    | Node (x,y) -> Some x
    | Leaf i -> None

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
