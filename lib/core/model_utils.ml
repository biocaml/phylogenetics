open Sigs

module Make (E:EVOL_MODEL) =
struct
  (* Setting up relevant data structures *)
  open LATools
  module Base = E.Base
  module Seq = Seq.Make (Base)
  module Align = Alignment.Make (Seq)

  type tree = TopoTree.t
  type align = Align.t
  type param = E.t

  let rate_matrix e = init_mat Base.alphabet_size
    @@ fun i j -> E.transition e (Base.of_int (i-1)) (Base.of_int (j-1))

  let stat_dist_vec e = init_vec Base.alphabet_size
    @@ fun i -> E.stat_dist e (Base.of_int (i-1))

  let diag_p e = init_mat Base.alphabet_size
    @@ fun i j -> E.diag_p e i j

  let diag_p_inv e = init_mat Base.alphabet_size
    @@ fun i j -> E.diag_p_inv e i j

  let diag e =
    let d = E.diag e in
    fun t ->
      init_mat Base.alphabet_size
      @@ fun i j -> if i=j then d i |> ( *. ) t |> Pervasives.exp else 0.0

  let eMt_series e t = exp (scal_mat_mul (rate_matrix e) t)

  let eMt e =
    let d, d_p, d_pi = diag e, diag_p e, diag_p_inv e in
    fun t -> mult (mult d_p (d t)) d_pi

  let known_vector b = init_vec Base.alphabet_size
    @@ fun x->if x=Base.to_int b + 1 then 1. else 0.
end
