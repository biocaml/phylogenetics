(** Compilation of modules implementing evolution models and
    providing relevant mathematical procedure (eg, exponential of transition matrix);
    also includes functors to build models from transition matrices.*)

open Linear_algebra_tools

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

(** Jukes-Cantor model with analytical diagonalization of transition matrix. *)
module JC69 : S with type t = unit

(** Jukes-Cantor with numerical diagonalization of transition matrix *)
module JC69_generated : S with type t = unit

(** K80 model with analytical diagonalization of transition matrix (parametrized by kappa) *)
module K80 : S with type t = float

(** K80 model with numerical diagonalization of transition matrix
    (needs to recompute for every value of kappa) *)
module K80_generated : S with type t = float

(** Model + parameter bundle *)
type t = {
  model : (module S) ;
  param : string
}

(** Returns a model + parameter from a string specifying the model (bpp format).
    Eg, "K80(kappa=2.0)" returns module K80 and parameter 2.0.*)
val of_string: string -> t option
