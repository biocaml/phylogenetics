open Printf
open Sigs

module Felsenstein (B:BASE) (E:EVOL_MODEL with type base=B.t) =
struct
  open TopoTree
  open LATools

  module Align = Alignment.Make (Sequence.Make (B))

  let transition_of_int e x y =
    E.transition e (B.of_int (x-1)) (B.of_int (y-1))

  let rate_matrix e = init_mat 4 (transition_of_int e)

  let stat_dis_vec e = init_vec 4 (fun x -> E.stat_dis e (B.of_int (x-1)))

  let eMt e t = exp (scal_mat_mult (rate_matrix e) t)

  let known_vector b =
    init_vec 4 (fun x->if x=B.to_int b + 1 then 1. else 0.)

  let felsenstein e t (sequences:Align.t) =
    let rec aux tr = match tr with
      | Node ((f1,l), (f2,r)) ->
        vec_vec_mul
          (mat_vec_mul (eMt e f1) (aux l))
          (mat_vec_mul (eMt e f2) (aux r))
      | Leaf i -> known_vector (Align.get_base sequences ~seq:i ~pos:0)
    in let res = aux t in
    begin
      (* print_vec res ; *)
      let myvec = stat_dis_vec e |> vec_vec_mul res in
      sum_vec_elements myvec
    end
end


(* ========================
   ||                    ||
   ||       TESTS        ||
   ||                    ||
   ======================== *)
module JCFelsenstein = Felsenstein (Nucleotide) (Models.JC69)

let test () =
  let mytree = TopoTree.tree_of_string "0.0895312;0.0576168;1;0" in
  let myseq = ["C";"C"] |> JCFelsenstein.Align.of_string_list in
  begin
    JCFelsenstein.felsenstein () mytree myseq |> log
    |> printf "Returns: %F\nBio++..: -1.52971733717731\n" ;
  end

let test2 () =
  let mytree = TopoTree.tree_of_string "0.1;0.1;1;0" in
  let myseq = ["C";"G"] |> JCFelsenstein.Align.of_string_list in
  begin
    JCFelsenstein.felsenstein () mytree myseq |> log
    |> printf "Returns.: %F\nBio++...: -4.22471668644312\nHandbook: -4.21922774436879067\n" ;
  end
