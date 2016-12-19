open Printf
open Sigs
open Core_kernel.Std

module Felsenstein (E:EVOL_MODEL) =
struct
  open TopoTree
  open LATools

  module Base = E.Base
  module Align = Alignment.Make (Seq.Make (Base))

  let rate_matrix e = init_mat Base.alphabet_size
    @@ fun x y -> E.transition e (Base.of_int (x-1)) (Base.of_int (y-1))

  let stat_dis_vec e = init_vec Base.alphabet_size
    @@ fun x -> E.stat_dis e (Base.of_int (x-1))

  let diag_p _ = init_mat Base.alphabet_size
    @@ fun x y -> E.diag_p x y

  let diag_p_inv _ = init_mat Base.alphabet_size
    @@ fun x y -> E.diag_p_inv x y

  let diag _ t = init_mat Base.alphabet_size
    @@ fun x y -> if x=y then E.diag x |> ( *. ) t |> Pervasives.exp else 0.0

  let eMt e t =
    if E.has_decomposition then
      mult (mult (diag_p e) (diag e t)) (diag_p_inv e)
    else
      exp (scal_mat_mul (rate_matrix e) t)

  let known_vector b = init_vec Base.alphabet_size
    @@ fun x->if x=Base.to_int b + 1 then 1. else 0.


  (* ======================= *)
  (* | Generic Felsenstein | *)
  (* ======================= *)
  let felsenstein
      ?(shift=fun _ _ v->v,0.0)
      ?(combine=vec_vec_mul)
      ?(in_f=Core_kernel.Std.ident)
      ?(out_f=Core_kernel.Std.ident)
      ~site
      () param tree seq
    =

    let rec aux tr =
      match tr with
      | Node ((f1,l), (f2,r)) -> node f1 l f2 r
      | Leaf i -> leaf i

    and leaf i = Align.get_base seq ~seq:i ~pos:site
                 |> known_vector |> in_f |> shift 0.0 0.0

    and node f1 l f2 r = match aux l, aux r with (v_l, s_l), (v_r, s_r) ->
      combine
        (mat_vec_mul (eMt param f1) (out_f v_l) |> in_f)
        (mat_vec_mul (eMt param f2) (out_f v_r) |> in_f)
      |> shift s_l s_r

    in let statdis = stat_dis_vec param |> in_f in
    let res_vec, res_shift = aux tree in
    res_vec |> combine statdis |> out_f |> sum_vec_elements |> log |> (+.) res_shift


  (* ============================ *)
  (* | Specific implementations | *)
  (* ============================ *)
  let shift_log thre acc1 acc2 v =
    if min_vec v > thre then (v, acc1 +. acc2)
    else
      let mv = max_vec v in
      (scal_vec_add v (0.0 -. mv), acc1 +. acc2 +. mv)

  let felsenstein_logshift ?threshold:(threshold=(-1.0)) ~site () =
    felsenstein
      ~shift:(shift_log threshold)
      ~combine:vec_vec_add
      ~in_f:log_vec ~out_f:unlog_vec
      ~site:site ()

  let shift_normal thre acc1 acc2 v =
    if min_vec v > thre then (v, acc1 +. acc2)
    else
      let mv = max_vec v in
      (scal_vec_mul v (1.0 /. mv), acc1 +. acc2 +. (log mv))

  let felsenstein_shift ?threshold:(threshold=0.1) ~site () =
    felsenstein
      ~shift:(shift_normal threshold)
      ~site:site ()


  (* ======================= *)
  (* | Multi-site versions | *)
  (* ======================= *)
  let multisite (f: site:int -> E.t -> TopoTree.t -> Align.t -> float) param tree seq =
    let l = Align.length seq in
    List.fold (List.range 0 l) ~init:0.0 ~f:(fun acc x -> (f ~site:x param tree seq) +. acc)

  let multi_felsenstein () = multisite (felsenstein ())
  let multi_felsenstein_shift () = multisite (felsenstein_shift ())
  let multi_felsenstein_logshift () = multisite (felsenstein_logshift ())

end


(* ========================
   ||                    ||
   ||       TESTS        ||
   ||                    ||
   ======================== *)
module JCFelsenstein = Felsenstein (Models.JC69)

let test () =
  let mytree = TopoTree.of_preorder "0.0895312;0.0576168;1;0" in
  let myseq = ["C";"C"] |> JCFelsenstein.Align.of_string_list in
  begin
    JCFelsenstein.felsenstein ~site:0 () () mytree myseq
    |> printf "Returns: %F\nBio++..: -1.52971733717731\n" ;
  end

let test2 () =
  let mytree = TopoTree.of_preorder "0.1;0.1;1;0" in
  let myseq = ["C";"G"] |> JCFelsenstein.Align.of_string_list in
  begin
    JCFelsenstein.felsenstein ~site:0 () () mytree myseq
    |> printf "Normal..: %F\n" ;
    JCFelsenstein.felsenstein_logshift ~site:0 () () mytree myseq
    |> printf "Log.....: %F\nBio++...: -4.22471668644312\nHandbook: -4.21922774436879067\n"
  end

let mytree = TopoTree.of_preorder
    "0.21;0.1;0.3;0.4;0;0.8;0.1;1;2;0.12;0.9;3;0.2;0.3;0.3;0.4;4;5;6"

let myseq =
  ["CA";"GA";"CC";"TA";"AC";"TC";"GA"]
  |> JCFelsenstein.Align.of_string_list


let test3 () =
  begin
    JCFelsenstein.felsenstein ~site:0 () () mytree myseq
    |> printf "Normal..: %F\n" ;
    JCFelsenstein.felsenstein_logshift ~site:0 () () mytree myseq
    |> printf "LogShift: %F\n" ;
    JCFelsenstein.felsenstein_shift ~site:0 () () mytree myseq
    |> printf "Shift...: %F\n"
  end
