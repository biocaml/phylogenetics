(** Compilation of modules implementing evolution models and
    providing relevant mathematical procedure (eg, exponential of transition matrix);
    also includes functors to build models from transition matrices.*)

(** Evolution model with linear algebra functions to compute
   stationary distribution and transition matrix diagonalization.*)
module type S = sig
  type param
  type vec
  type mat
  val rate_matrix : param -> mat
  val transition_probability_matrix : param -> float -> mat
  val stationary_distribution : param -> vec
end

module type S_with_reduction = sig
  include S
  val rate_matrix_reduction : param -> mat * vec * mat
end

module type Rate_matrix = sig
  type param
  type mat
  val rate_matrix : param -> mat
end

module type Diagonalizable_rate_matrix = sig
  include Rate_matrix
  type vec
  val rate_matrix_reduction : param -> mat * vec * mat
end

module Make
    (A : Alphabet.S)
    (M : Rate_matrix with type mat := A.matrix) :
sig
  val transition_probability_matrix : M.param -> float -> A.matrix
  val stationary_distribution : M.param -> A.vector
end

module Make_diag
    (A : Alphabet.S)
    (M : Diagonalizable_rate_matrix with type vec := A.vector
                                     and type mat := A.matrix) :
sig
  val transition_probability_matrix : M.param -> float -> A.matrix
  val stationary_distribution : M.param -> A.vector
end

module type Nucleotide_S_with_reduction =
  S_with_reduction
  with type vec := Nucleotide.vector
   and type mat := Nucleotide.matrix

(** Jukes-Cantor model with analytical diagonalization of transition
   matrix. *)
module JC69 : Nucleotide_S_with_reduction with type param = unit

(** Jukes-Cantor model with numerical calculation of probability
   transition matrix *)
module JC69_numerical : Nucleotide_S_with_reduction with type param = unit

(** K80 model with analytical diagonalization of transition matrix (parametrized by kappa) *)
module K80 : Nucleotide_S_with_reduction with type param = float

(** K80 model with numerical calculation of probability
   transition matrix *)
module K80_numerical : Nucleotide_S_with_reduction with type param = float

module Amino_acid_GTR : sig
  type t = {
    stationary_distribution : Amino_acid.vector ;
    exchangeability_matrix : Amino_acid.matrix ;
    scale : float ;
  }

  val rate_matrix : t -> Rate_matrix.Amino_acid.t

  val transition_matrix : t -> float -> Amino_acid.matrix
end
