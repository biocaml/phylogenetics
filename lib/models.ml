open Printf

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
module type EVOL_MODEL = sig
  (* include EVOL_BASE*)
  include Sequence.SEQUENCE
  val transition: base -> base -> float
  val stat_dis: base -> float
end

module type FELSENSTEIN = sig
  include EVOL_MODEL
  type tree = TopoTree.tree
  val felsenstein: tree -> sequence_table -> float
end


(* ========================
   ||                    ||
   ||    BASIC MODELS    ||
   ||                    ||
   ======================== *)
module JCModel:EVOL_MODEL =
struct
  include Sequence.DNA_Sequence
  let transition a b = if a=b then -1.0 else 1./.3. (*not sure why this convention*)
  let stat_dis a = 0.25
end


(* ========================
   ||                    ||
   ||      FUNCTORS      ||
   ||                    ||
   ======================== *)

module Felsenstein (Mod: EVOL_MODEL):FELSENSTEIN =
struct
  open TopoTree
  open LATools
  include Mod

  type tree = TopoTree.tree

  let transition_of_int x y =
    transition (base_of_int (x-1)) (base_of_int (y-1))

  let rate_matrix () = init_mat 4 transition_of_int

  let stat_dis_vec () = init_vec 4 (fun x -> Mod.stat_dis (Mod.base_of_int (x-1)))

  let eMt t = exp (scal_mat_mult (rate_matrix ()) t)

  let known_vector b =
    init_vec 4 (fun x->if x=(int_of_base b + 1) then 1. else 0.)

  let felsenstein t sequences =
    let rec aux tr = match tr with
      | Node ((f1,l), (f2,r)) -> vec_vec_mul
                                   (mat_vec_mul (eMt f1) (aux l))
                                   (mat_vec_mul (eMt f2) (aux r))
      | Leaf i -> known_vector (get_base i 0 sequences)
    in let res = aux t in
    begin
      (* print_vec res ; *)
      let myvec = stat_dis_vec () |> vec_vec_mul res in
      sum_vec_elements myvec
    end

  (* ========= *)
  (*   TESTS   *)
  (* ========= *)
  let test b1 b2 =
    let printline () = print_string "==========================\n" in
    printline ();
    Printf.printf "%F %F\n" (Mod.transition b1 b1) (Mod.transition b1 b2);
    printline () ;
    LATools.print_mat (rate_matrix ()); Printf.printf "\n" ;
    printline () ;
    LATools.print_mat (eMt 1.2) ;
    printline ()
end


(* ========================
   ||                    ||
   ||       TESTS        ||
   ||                    ||
   ======================== *)
module JCFelsenstein = Felsenstein (JCModel)

let test () =
  let mytree = TopoTree.tree_of_string "0.0895312;0.0576168;1;0" in
  let myseq = ["C";"C"] |> JCFelsenstein.table_of_string_list in
  begin
    JCFelsenstein.felsenstein mytree myseq |> log
    |> printf "Returns: %F\nBio++..: -1.52971733717731\n" ;
  end

let test2 () =
  let mytree = TopoTree.tree_of_string "0.1;0.1;1;0" in
  let myseq = ["C";"G"] |> JCFelsenstein.table_of_string_list in
  begin
    JCFelsenstein.felsenstein mytree myseq |> log
    |> printf "Returns.: %F\nBio++...: -4.22471668644312\nHandbook: -4.21922774436879067\n" ;
  end
