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

  (* takes a vector in log space ;
     if it is too small (below threshold t) then
     it shifts it by the max absolute value and returns
     shifted_vector, shift*)
  let shift t acc v =
    let min_e = min_vec v in
    if min_e > t then (v, acc)
    else
      (scal_vec_add v min_e, acc -. min_e)

  let felsenstein_log param tree seq =
    let rec aux tr =
      match tr with
      | Node ((f1,l), (f2,r)) -> node f1 l f2 r
      | Leaf i -> leaf i

    and leaf i = Align.get_base seq ~seq:i ~pos:0 |> known_vector |> log_vec, 0.

    and node f1 l f2 r = match aux l, aux r with (v_l, s_l), (v_r, s_r) ->
      vec_vec_add
        (mat_vec_mul (eMt param f1) (unlog_vec v_l) |> log_vec)
        (mat_vec_mul (eMt param f2) (unlog_vec v_r) |> log_vec)
      |> shift (-100.0) (s_l +. s_r)

    in let statdis = stat_dis_vec param |> log_vec in
    match aux tree with (x,y) ->
      scal_vec_add x y |> vec_vec_add statdis |> unlog_vec
      |> sum_vec_elements
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
    JCFelsenstein.felsenstein () mytree myseq
    |> printf "Returns: %F\nBio++..: -1.52971733717731\n" ;
  end

let test2 () =
  let mytree = TopoTree.of_string "0.1;0.1;1;0" in
  let myseq = ["C";"G"] |> JCFelsenstein.Align.of_string_list in
  begin
    JCFelsenstein.felsenstein () mytree myseq
    |> printf "Normal..: %F\n" ;
    JCFelsenstein.felsenstein_log () mytree myseq
    |> printf "Log.....: %F\nBio++...: -4.22471668644312\nHandbook: -4.21922774436879067\n"
  end

let mytree = TopoTree.of_string
    "0.21;0.1;0.3;0.4;0;0.8;0.1;1;2;0.12;0.9;3;0.2;0.3;0.3;0.4;4;5;6"

let myseq =
  ["C";"G";"C";"T";"A";"T";"G"]
  |> JCFelsenstein.Align.of_string_list


let test3 () =
  begin
    JCFelsenstein.felsenstein () mytree myseq
    |> printf "Normal..: %F\n" ;
    JCFelsenstein.felsenstein_log () mytree myseq
    |> printf "Log.....: %F\n"
  end
