open Sigs
open Core_kernel.Std

module Seqgen (E:EVOL_MODEL) =
struct

  (* Setting up relevant data structures *)
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

  let proba param base t =
    mat_vec_mul (eMt param t) (known_vector base)

  let draw_base vec =
    let open Base in
    (* for all base check if x is smaller than transition proba,
       if yes return base else decrement x *)
    let rec aux i x =
      let proba = get_vec vec i in
      if x < proba then of_int (i-1)
      else aux (i+1) (x-.proba)
    in
    Random.float 1.0 |> aux 1

  let seqgen = ()
end

module JCSeqgen = Seqgen (Models.JC69)

let test () =
  JCSeqgen.draw_base (JCSeqgen.proba () (JCSeqgen.Base.of_int 0) 0.7)
  |> JCSeqgen.Base.to_string
