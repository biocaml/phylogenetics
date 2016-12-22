open Sigs

module Model_utils (E:EVOL_MODEL) =
struct
  (* Setting up relevant data structures *)
  open TopoTree
  open LATools
  module Base = E.Base
  module Seq = Seq.Make (Base)
  module Align = Alignment.Make (Seq)

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

end
