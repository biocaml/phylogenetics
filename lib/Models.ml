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
    open LATools

    let transition_of_int x y =
      Mod.transition (Mod.base_of_int (x-1)) (Mod.base_of_int (y-1))

    let rate_matrix () = init 4 transition_of_int

    let eMt t = exp (scalmul (rate_matrix ()) t)

    let known_vector b =
      initvec 4 (fun x->if x=(int_of_dna b + 1) then 1. else 0.)

    let rec felsenstein t sequences =
      let rec aux tr = match tr with
        | Node ((f1,l), (f2,r)) -> vec_vec_mul
                                     (mat_vec_mul (eMt f1) (aux l))
                                     (mat_vec_mul (eMt f2) (aux r))
        | Leaf i ->
          known_vector (Sequence.get_base i sequences)
      in let res = aux t in
      begin
        printVec res ;
        sum_vec_elements res
      end

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
  let mytree =
    match
      TopoTree.tree_of_string "1.23357;0.0223917;0.157039;0.0431535;3;4;0.133751;0.0661129;2;0.121775;0.123267;1;0"
    with
      Ok t -> t | Error e -> TopoTree.Leaf 0 in
  begin
    ignore (JCFelsenstein.felsenstein mytree [(0,A);(1,T);(2,C);(3,G);(4,C)]) ;
    (* JCFelsenstein.test A T; *)
    (* TopoTree.pretty_print mytree *)
  end

let t2 () =
  let mytree = match TopoTree.tree_of_string "0.1;0.1;0.1;0.1;1;2;3;4"
    with Ok t -> t | Error _ -> TopoTree.Leaf 0 in
  let myseq = [(1,C);(2,G);(3,C);(4,C)] in
  ignore (JCFelsenstein.felsenstein mytree myseq)

let t3 () =
  JCFelsenstein.eMt 0.1
