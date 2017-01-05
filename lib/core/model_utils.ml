open Sigs

module Make (E:EVOL_MODEL) =
struct
  (* Setting up relevant data structures *)
  open TopoTree
  open LATools
  module Base = E.Base
  module Seq = Seq.Make (Base)
  module Align = Alignment.Make (Seq)

  type tree = TopoTree.t
  type align = Align.t
  type param = E.t

  let rate_matrix e = init_mat Base.alphabet_size
    @@ fun x y -> E.transition e (Base.of_int (x-1)) (Base.of_int (y-1))

  let stat_dist_vec e = init_vec Base.alphabet_size
    @@ fun x -> E.stat_dist e (Base.of_int (x-1))

  let diag_p e = init_mat Base.alphabet_size
    @@ fun x y -> E.diag_p e x y

  let diag_p_inv e = init_mat Base.alphabet_size
    @@ fun x y -> E.diag_p_inv e x y

  let diag e t = init_mat Base.alphabet_size
    @@ fun x y -> if x=y then E.diag e x |> ( *. ) t |> Pervasives.exp else 0.0

  let eMt_series e t = exp (scal_mat_mul (rate_matrix e) t)

  let eMt e t =
    if E.has_decomposition then
      mult (mult (diag_p e) (diag e t)) (diag_p_inv e)
    else
      eMt_series e t

  let known_vector b = init_vec Base.alphabet_size
    @@ fun x->if x=Base.to_int b + 1 then 1. else 0.

end
