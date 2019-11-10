(** This modules wraps linear algebra functions (from Lacaml)
    and provide a few completely new functions (such as exponentiation)*)

module Lacaml : sig
  (** A square matrix of floats. *)
  type mat = Lacaml.D.mat

  (** A vector of floats. *)
  type vec = Lacaml.D.vec

  module Vec : sig
    type t = vec

    (** Initialises a vector from a int->float function. *)
    val init : int -> f:(int -> float) -> vec

    (** Vector addition. *)
    val add : vec -> vec -> vec

    (** Element-wise product of two vectors. *)
    val mul : vec -> vec -> vec

    (** Sum of the elements of a vector. *)
    val sum : vec -> float

    (** Element-wise logarithm of vector *)
    val log : vec -> vec

    (** Element-wise exponential of matrix*)
    val exp : vec -> vec

    (** Minimum element in a vector. *)
    val min : vec -> float

    (** Maximum element in a vector. *)
    val max : vec -> float

    (** Access a specific element of a vector. *)
    val get : vec -> int -> float

    (** Prints a vector to the standard output. *)
    val pp : Format.formatter -> vec -> unit
  end

  module Mat : sig
    type t = mat

    (** {5 Matrix and vector creation} *)

    (** Initialises a square matrix from a int->int->float function. *)
    val init : int -> f:(int -> int -> float) -> mat

    (** Initializes a square diagonal matrix from the vector of its diagonal elements. *)
    val init_diag : vec -> mat

    (** Computes the product of two matrices.
        If optional argument alpha is provided then the result is also
        multiplied by scalar alpha.*)
    val mul : mat -> ?alpha:float -> mat -> mat

    (** Elevates a matrix to an integer power.
        If optional argument alpha is provided then the result is also
        multiplied by scalar alpha. *)
    val pow: mat -> ?alpha:float -> int -> mat

    (** Matrix addition. *)
    val add : mat -> mat -> mat

    (** Matrix exponentiation. *)
    val expm : mat -> mat

    (** Element-wise logarithm of matrix *)
    val log : mat -> mat

    (** Compares two matrices and tolerates a certain relative difference.
        Let f be the float parameter, it returns true iff the elements of the second matrix
        are between 1-f and 1+f times the corresponding elements of the first *)
    val compare: tol:float -> mat -> mat -> bool

    (** Access a specific element of a matrix. *)
    val get : mat -> int -> int -> float

    (** Copy row from a matrix *)
    val row : mat -> int -> vec

    (** Diagonalizes a matrix M so that M = PxDxP^T; returns P,v,P^T where
        v is the diagonal vector of D.*)
    val diagonalize : mat -> mat * vec * mat

    (** Computes the inverse of a matrix. *)
    val inverse: mat -> mat

    (** Prints a matrix to the standard output (display may be messy). *)
    val pp : Format.formatter -> mat -> unit
  end

  (** Matrix-vector product. *)
  val mat_vec_mul: mat -> vec -> vec

  (** Multiplication of a matrix by a scalar. *)
  val scal_mat_mul : float -> mat -> mat

  (** Scalar-vector product (in-place). *)
  val inplace_scal_vec_mul: float -> vec -> unit

  (** Scalar-vector product *)
  val scal_vec_mul : float -> vec -> vec

  (** Scalar-vector addition. *)
  val scal_vec_add: float -> vec -> vec

  (** Computes the static distribution (ie, eigenvector for eigenvalue 0) of a given matrix. *)
  val stat_dist: mat -> vec
end
