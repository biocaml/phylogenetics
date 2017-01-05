open Sigs

module JC69 = struct
  type t = unit
  module Base = Nucleotide
  let transition () a b = if a=b then -1.0 else 1./.3.
  let stat_dis () _ = 0.25
  let has_decomposition = true
  let diag () = function 1 -> 0.0 | _ -> -1.333333333333
  let diag_p () i j =
    if j=1 then 1.0
    else if i=1 then -1.0
    else if i=j then 1.0
    else 0.0
  let diag_p_inv () i j =
    if i=1 then 0.25
    else if i=j then 0.75
    else -0.25
  let of_string _ = ()
  let to_string _ = "JC69"
end

module K80 = struct
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
  let stat_dis _ _ = 0.25
  let has_decomposition = true
  let diag k = function
    | 1 -> 0.0
    | 2 -> (-4.)/.(k+.2.)
    | _ -> (-2.*.k-.2.)/.(k+.2.)
  let diag_p _ i j = match (i,j) with
    | (_,1) | (3,2) | (4,2) | (2,4) | (4,3) -> 1.0
    | (1,2) | (2,2) | (1,4) | (3,3) -> -1.0
    | _ -> 0.0
  let diag_p_inv _ i j = match (i,j) with
    | (1,_) | (2,3) | (2,4) -> 0.25
    | (2,_) -> -0.25
    | (3,4) | (4,2) -> 0.5
    | (3,3) | (4,1) -> -0.5
    | _ -> 0.0
  let of_string = float_of_string
  let to_string k = Printf.sprintf "K80(kappa=%f)" k
end

(** A record containing a model and a parameter *)
type t = {model:(module EVOL_MODEL) ; param:string}

(** Returns the module+parameters specified in a string using bpp format *)
let of_string str =
  if str = "JC69" then {model = (module JC69:EVOL_MODEL) ; param = ""}
  else {
    model = (module K80:EVOL_MODEL) ;
    param = Scanf.sscanf str "K80(kappa=%f)" (fun x->string_of_float x)
  }

(** If possible, creates a model module from a transition matrix *)
module Make (M:TRANSITION_MATRIX) = struct
  include M

  (* LATools versions of things for convenience *)
  let transition_mat p =
    LATools.init_mat
      (Base.alphabet_size)
      (fun i j -> transition p (Base.of_int (i-1)) (Base.of_int (j-1)) )
  let stat_dist_vec p = LATools.stat_dist (transition_mat p)
  let diagonalization_mats p = LATools.diagonalize (transition_mat p)

  let stat_dis p b = LATools.get_vec (stat_dist_vec p) ((Base.to_int b)+1)
  let has_decomposition = true
  let diag p i = match diagonalization_mats p with
    | (_,m,_) -> LATools.get_mat m i i
  let diag_p p i j = match diagonalization_mats p with
    | (m,_,_) -> LATools.get_mat m i j
  let diag_p_inv p i j = match diagonalization_mats p with
    | (_,_,m) -> LATools.get_mat m i j
end
