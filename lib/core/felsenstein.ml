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

  let shift_generic vec_op div sum thre acc1 acc2 v =
    if min_vec v > thre then (v, sum acc1 acc2)
    else
      let mv = max_vec v in
      (vec_op v (div mv), sum (sum acc1 acc2) mv)

  let unshift vec_op (v, acc) = vec_op v acc

  let id x = x


  (* ======================= *)
  (* | Generic felsenstein | *)
  (* ======================= *)
  let felsenstein
      ?shift:(shift=(fun x y z->z, 1.0))
      ?unshift:(unshift=(function (x,y)-> x))
      ?combine:(combine=vec_vec_mul)
      ?post:(post=id)
      ?pre:(pre=id)
      ?zero:(zero=1.0)
      param tree seq =

    let rec aux tr =
      match tr with
      | Node ((f1,l), (f2,r)) -> node f1 l f2 r
      | Leaf i -> leaf i

    and leaf i = Align.get_base seq ~seq:i ~pos:0
                 |> known_vector |> post |> shift zero zero

    and node f1 l f2 r = match aux l, aux r with (v_l, s_l), (v_r, s_r) ->
      combine
        (mat_vec_mul (eMt param f1) (pre v_l) |> post)
        (mat_vec_mul (eMt param f2) (pre v_r) |> post)
      |> shift s_l s_r

    in let statdis = stat_dis_vec param in
    aux tree |> unshift |> pre |> vec_vec_mul statdis |> sum_vec_elements |> log


  (* ============================ *)
  (* | Specific implementations | *)
  (* ============================ *)
  let felsenstein_logshift ?threshold:(threshold=(-1.0)) =
    felsenstein
      ~shift:(shift_generic scal_vec_add ((-.) 0.) (+.) threshold)
      ~unshift:(unshift scal_vec_add)
      ~combine:vec_vec_add
      ~post:log_vec ~pre:unlog_vec ~zero:0.0

  let felsenstein_shift ?threshold:(threshold=0.1) =
    felsenstein
      ~shift:(shift_generic scal_vec_mul ((/.) 1.) ( *. ) threshold)
      ~unshift:(unshift scal_vec_mul)
      ~combine:vec_vec_mul
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
    JCFelsenstein.felsenstein_logshift () mytree myseq
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
    JCFelsenstein.felsenstein_logshift () mytree myseq
    |> printf "LogShift: %F\n" ;
    JCFelsenstein.felsenstein_shift () mytree myseq
    |> printf "Shift...: %F\n"
  end
