open Sigs
open Core_kernel.Std

module Seqgen (E:EVOL_MODEL) =
struct
  module Utils = Model_utils.Model_utils (E)
  include Utils
  open LATools

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
