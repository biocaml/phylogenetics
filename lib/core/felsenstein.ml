open Printf
open Sigs

module Felsenstein (E:EVOL_MODEL) =
struct
  open TopoTree
  open LATools

  module Base = E.Base
  module Align = Alignment.Make (Sequence.Make (Base))

  let rate_matrix e = init_mat Base.alphabet_size
    @@ fun x y -> E.transition e (Base.of_int (x-1)) (Base.of_int (y-1))

  let stat_dis_vec e = init_vec Base.alphabet_size
    @@ fun x -> E.stat_dis e (Base.of_int (x-1))

  let eMt e t = exp (scal_mat_mul (rate_matrix e) t)

  let known_vector b = init_vec Base.alphabet_size
    @@ fun x->if x=Base.to_int b + 1 then 1. else 0.

  let felsenstein e t (sequences:Align.t) =
    let rec aux tr = match tr with
      | Node ((f1,l), (f2,r)) ->
        vec_vec_mul
          (mat_vec_mul (eMt e f1) (aux l))
          (mat_vec_mul (eMt e f2) (aux r))
      | Leaf i ->
        Align.get_base sequences ~seq:i ~pos:0
        |> known_vector
    in let res = aux t in
    stat_dis_vec e |> vec_vec_mul res |> sum_vec_elements

  let felsenstein_log param tree seq =
    let rec aux tr = match tr with
      | Node ((f1,l), (f2,r)) ->

        vec_vec_add
          (mat_vec_mul (eMt param f1) (aux l |> unlog_vec) |> log_vec)
          (mat_vec_mul (eMt param f2) (aux r |> unlog_vec) |> log_vec)

      | Leaf i ->
        Align.get_base seq ~seq:i ~pos:0
        |> known_vector |> log_vec
    in let statdis = stat_dis_vec param |> log_vec in
    aux tree |> vec_vec_add statdis |> unlog_vec |> sum_vec_elements |> log
end


(* ========================
   ||                    ||
   ||       TESTS        ||
   ||                    ||
   ======================== *)
module JCFelsenstein = Felsenstein (Models.JC69)

let test () =
  let mytree = TopoTree.of_string "0.0895312;0.0576168;1;0" in
  let myseq = ["C";"C"] |> JCFelsenstein.Align.of_string_list in
  begin
    JCFelsenstein.felsenstein () mytree myseq |> log
    |> printf "Returns: %F\nBio++..: -1.52971733717731\n" ;
  end

let test2 () =
  let mytree = TopoTree.of_string "0.1;0.1;1;0" in
  let myseq = ["C";"G"] |> JCFelsenstein.Align.of_string_list in
  begin
    JCFelsenstein.felsenstein () mytree myseq |> log
    |> printf "Normal..: %F\n" ;
    JCFelsenstein.felsenstein_log () mytree myseq
    |> printf "Log.....: %F\nBio++...: -4.22471668644312\nHandbook: -4.21922774436879067\n"
  end
