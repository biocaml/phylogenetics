open Linear_algebra_tools

(* =================================== *)
(*  SIGNATURES FOR FUNCTOR PARAMETERS  *)
(* =================================== *)

(** Module type for transition matrix dependent on a parameter type. *)
module type TRANSITION_MATRIX = sig
  type t
  type base
  val transition: t -> base -> base -> float
  val of_string: string -> t
  val to_string: t -> string
end

module type MODEL_WITH_DIAG = sig
  include TRANSITION_MATRIX
  val transition_mat: t -> mat
  val stat_dist_vec: t -> vec
  val diag_mats: t -> mat * (float -> mat) * mat
end

(** Evolution model with linear algebra functions to compute static distribution and
    transition matrix diagonalization.*)
module type S = sig
  type t
  val transition: t -> Nucleotide.t -> Nucleotide.t -> float
  val of_string: string -> t
  val to_string: t -> string
  val eMt_mat: t -> float -> mat
  val eMt_series: t -> float -> mat
  val stat_dist_vec: t -> vec
  val known_vector: Nucleotide.t -> vec
end

(** A record containing a model and a parameter *)
type t = {
  model : (module S) ;
  param : string
}

(* ========== *)
(*  FUNCTORS  *)
(* ========== *)

module type Base = sig
  include Alphabet.S_int
  include Seq.Base with type t := t
end

(** If possible, creates a model module from a transition matrix *)
module Make_exp(Base : Base)(E : MODEL_WITH_DIAG with type base := Base.t) =
struct
  include E
  module Seq = Seq.Make(Base)
  module Align = Alignment.Make(Seq)
  let eMt_series e t = exp (scal_mat_mul (E.transition_mat e) t)
  let eMt_mat e =
    let diag_p, diag, diag_p_inv = E.diag_mats e in
    fun t -> mult (mult diag_p (diag t)) diag_p_inv
  let known_vector b = init_vec Base.card
    @@ fun x -> if x = Base.to_int b + 1 then 1. else 0.
end

module Make(Base : Base)(M : TRANSITION_MATRIX with type base := Base.t) = struct
  module Diag = struct
    include M
    (* LATools versions of things for convenience *)
    let transition_mat p =
      init_mat Base.card (fun i j ->
          transition p (Base.of_int_exn (i-1)) (Base.of_int_exn (j-1))
        )
    let stat_dist_vec p = stat_dist (transition_mat p)
    let diag_mats p =
      match diagonalize (transition_mat p) with
        a, b, c -> a, (fun t -> init_diag (scal_vec_mul_cpy b t |> unlog_vec)), c
  end

  include Make_exp(Base)(Diag)
end

(* ================= *)
(*  SPECIFIC MODELS  *)
(* ================= *)

module JC69_mat = struct
  type t = unit
  let transition () a b = if a=b then -1.0 else 1./.3.
  let of_string _ = ()
  let to_string _ = "JC69"
end

module JC69 =
  Make_exp(Nucleotide)(struct
    include JC69_mat
    let transition_mat () = init_mat 4 (fun i j ->
        transition () (Nucleotide.of_int_exn (i-1)) (Nucleotide.of_int_exn (j-1)) )
    let stat_dist_vec () = init_vec 4 (fun _->0.25)
    let diag_mats () =
      init_mat 4 (fun i j ->
          if j=1 then 1.0
          else if i=1 then -1.0
          else if i=j then 1.0
          else 0.0),
      (fun t -> init_mat 4 (fun i j ->
           if i = j then match i with 1 -> 1.0 | _ -> Stdlib.exp (t *. -4./.3.)
           else 0.0)),
      init_mat 4 (fun i j ->
          if i=1 then 0.25
          else if i=j then 0.75
          else -0.25)
  end)

module JC69_generated = Make(Nucleotide)(JC69_mat)

module K80_mat = struct
  let transversion x y =
    match Nucleotide.(inspect x, inspect y) with
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
  Make_exp(Nucleotide)(struct
    include K80_mat
    let transition_mat k = init_mat 4 (fun i j ->
        transition k (Nucleotide.of_int_exn (i-1)) (Nucleotide.of_int_exn (j-1)) )
    let stat_dist_vec _ = init_vec 4 (fun _->0.25)
    let diag_mats k =
      init_mat 4 (fun i j -> match (i,j) with
          | (_,1) | (2,2) | (3,4) | (4,2) | (4,3) -> 1.0
          | (1,2) | (2,3) | (1,4) | (3,2) -> -1.0
          | _ -> 0.0),
      (fun t -> init_mat 4 (fun i j ->
           if i=j then match i with
             | 1 -> 1.0
             | 2 -> Stdlib.exp (t *. (-4.) /. (k +. 2.))
             | _ -> Stdlib.exp (t *. (-2. *. k -. 2.) /. (k +. 2.))
           else 0.0)),
      init_mat 4 (fun i j -> match (i,j) with
          | (1,_) | (2,2) | (2,4) -> 0.25
          | (2,_) -> -0.25
          | (3,4) | (4,3) -> 0.5
          | (3,2) | (4,1) -> -0.5
          | _ -> 0.0)
  end)

module K80_generated = Make(Nucleotide)(K80_mat)

(** Returns the module+parameters specified in a string using bpp format *)
let models = [
  ("JC69_generated", (module JC69_generated : S), (fun _ -> ""));
  ("JC69", (module JC69 : S), (fun _ -> ""));
  ("K80(", (module K80 : S),
   fun x -> Scanf.sscanf x "K80(kappa=%f)" (fun x -> string_of_float x));
  ("K80_generated(", (module K80_generated : S),
   fun x -> Scanf.sscanf x "K80_generated(kappa=%f)" (fun x -> string_of_float x))
]

let of_string str =
  Base.List.find_map models ~f:(fun (name, model, param) ->
      if try String.sub str 0 (String.length name) = name with _ -> false
      then Some { model ; param = param str }
      else None
    )
