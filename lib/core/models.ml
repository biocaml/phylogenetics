open Sigs
open LATools

(* =================================== *)
(*  SIGNATURES FOR FUNCTOR PARAMETERS  *)
(* =================================== *)

(** Module type for transition matrix dependent on a parameter type. *)
module type TRANSITION_MATRIX = sig
  type t
  module Base:BASE
  val transition: t -> Base.t -> Base.t -> float
  val of_string: string -> t
  val to_string: t -> string
end

module type MODEL_WITH_DIAG = sig
  include TRANSITION_MATRIX
  val transition_mat: t -> mat
  val stat_dist_vec: t -> vec
  val diag_mats: t -> mat * (float -> mat) * mat
end

(** A record containing a model and a parameter *)
type t = {model:(module EVOL_MODEL) ; param:string}


(* ========== *)
(*  FUNCTORS  *)
(* ========== *)

(** If possible, creates a model module from a transition matrix *)
module Make_exp (E:MODEL_WITH_DIAG) =
struct
  include E
  let eMt_series e t = exp (scal_mat_mul (E.transition_mat e) t)
  let eMt_mat e t =
    let diag_p, diag, diag_p_inv = E.diag_mats e in
    mult (mult diag_p (diag t)) diag_p_inv
  let known_vector b = init_vec Base.alphabet_size
    @@ fun x -> if x = Base.to_int b + 1 then 1. else 0.
end

module Make (M:TRANSITION_MATRIX) = struct
  module Diag = struct
    include M
    (* LATools versions of things for convenience *)
    let transition_mat p =
      LATools.init_mat
        (Base.alphabet_size)
        (fun i j -> transition p (Base.of_int (i-1)) (Base.of_int (j-1)) )
    let stat_dist_vec p = LATools.stat_dist (transition_mat p)
    let diag_mats p =
      match LATools.diagonalize (transition_mat p) with
      a, b, c -> a, (fun t -> LATools.init_diag (scal_vec_mul b t |> unlog_vec)), c
  end

  include Make_exp (Diag)
end


(* ================= *)
(*  SPECIFIC MODELS  *)
(* ================= *)

module JC69_mat = struct
  type t = unit
  module Base = Nucleotide
  let transition () a b = if a=b then -1.0 else 1./.3.
  let of_string _ = ()
  let to_string _ = "JC69"
end

module JC69 =
  Make_exp (struct
    include JC69_mat
    let transition_mat () = init_mat 4 (fun i j ->
        transition () (Base.of_int (i-1)) (Base.of_int (j-1)) )
    let stat_dist_vec () = init_vec 4 (fun _->0.25)
    let diag_mats () =
      init_mat 4 (fun i j ->
          if j=1 then 1.0
          else if i=1 then -1.0
          else if i=j then 1.0
          else 0.0),
      (fun t -> init_mat 4 (fun i j ->
          if i = j then match i with 1 -> 1.0 | _ -> Pervasives.exp (t *. -4./.3.)
          else 0.0)),
      init_mat 4 (fun i j ->
          if i=1 then 0.25
          else if i=j then 0.75
          else -0.25)
  end)

module JC69_generated = Make (JC69_mat)

module K80_mat = struct
  module Base = Nucleotide
  let transversion a b =
    let open Base in
    match (a, b) with
    | (A,G) | (G,A) | (T,C) | (C,T) -> false
    | _ -> true
  type t = float (* transition/transversion rate *)
  let transition k a b =
    if a=b then -1.0 (* diagonal *)
    else if transversion a b then (1./.(k+.2.))
    else k/.(k+.2.)
  let of_string = float_of_string
  let to_string k = Printf.sprintf "K80(kappa=%f)" k
end


module K80 =
  Make_exp (struct
    include K80_mat
    let transition_mat k = init_mat 4 (fun i j ->
        transition k (Base.of_int (i-1)) (Base.of_int (j-1)) )
    let stat_dist_vec _ = init_vec 4 (fun _->0.25)
    let diag_mats k =

      init_mat 4 (fun i j -> match (i,j) with
          | (_,1) | (3,2) | (4,2) | (2,4) | (4,3) -> 1.0
          | (1,2) | (2,2) | (1,4) | (3,3) -> -1.0
          | _ -> 0.0),
      (fun t -> init_mat 4 (fun i j ->
          if i=j then match i with
            | 1 -> 1.0
            | 2 -> Pervasives.exp (t *. (-4.)/.(k+.2.))
            | _ -> Pervasives.exp (t *. (-2.*.k-.2.)/.(k+.2.))
          else 0.0)),
      init_mat 4 (fun i j -> match (i,j) with
          | (1,_) | (2,3) | (2,4) -> 0.25
          | (2,_) -> -0.25
          | (3,4) | (4,2) -> 0.5
          | (3,3) | (4,1) -> -0.5
          | _ -> 0.0)
  end)

module K80_generated = Make (K80_mat)


(** Returns the module+parameters specified in a string using bpp format *)
let of_string str =
  if str = "JC69" then {model = (module JC69:EVOL_MODEL) ; param = ""}
  else {
    model = (module K80:EVOL_MODEL) ;
    param = Scanf.sscanf str "K80(kappa=%f)" (fun x -> string_of_float x)
  }
