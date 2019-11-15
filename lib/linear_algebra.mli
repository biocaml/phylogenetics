(** This modules wraps linear algebra functions *)

(** A vector of floats. *)
module type Vector = sig
  type t

  (** Initialises a vector from a int->float function. *)
  val init : int -> f:(int -> float) -> t

  val map : t -> f:(float -> float) -> t

  (** Scalar-vector product (in-place). *)
  val inplace_scal_mul: float -> t -> unit

  (** Scalar-vector product *)
  val scal_mul : float -> t -> t

  (** Scalar-vector addition. *)
  val scal_add: float -> t -> t

  (** Vector addition. *)
  val add : t -> t -> t

  (** Element-wise product of two vectors. *)
  val mul : t -> t -> t

  (** Sum of the elements of a vector. *)
  val sum : t -> float

  (** Element-wise logarithm of vector *)
  val log : t -> t

  (** Element-wise exponential of matrix*)
  val exp : t -> t

  (** Minimum element in a vector. *)
  val min : t -> float

  (** Maximum element in a vector. *)
  val max : t -> float

  (** Access a specific element of a vector. *)
  val get : t -> int -> float

  (** Set a specific element of a vector. *)
  val set : t -> int -> float -> unit

  val of_array : float array -> t
  val to_array : t -> float array

  (** Prints a vector to the standard output. *)
  val pp : Format.formatter -> t -> unit
end

(** A square matrix of floats. *)
module type Matrix = sig
  type vec
  type t

  (** {5 Matrix and vector creation} *)

  (** Initialises a square matrix from a int->int->float function. *)
  val init : int -> f:(int -> int -> float) -> t

  (** Initializes a square diagonal matrix from the vector of its diagonal elements. *)
  val diagm : vec -> t

  (** Matrix element-wise multiplication *)
  val mul : t -> t -> t

  (** Matrix addition. *)
  val add : t -> t -> t

  (** Multiplication of a matrix by a scalar. *)
  val scal_mul : float -> t -> t

  (** Inplace multiplication of a matrix by a scalar. *)
  val inplace_scal_mul: float -> t -> unit

  (** Matrix multiplication *)
  val dot : t -> t -> t

  (** Matrix-vector product *)
  val apply : t -> vec -> vec

  (** Matrix exponentiation. *)
  val expm : t -> t

  (** Element-wise logarithm of matrix *)
  val log : t -> t

  (** Compares two matrices and tolerates a certain relative difference.
      Let f be the float parameter, it returns true iff the elements of the second matrix
      are between 1-f and 1+f times the corresponding elements of the first *)
  val compare: tol:float -> t -> t -> bool

  (** Access a specific element of a matrix. *)
  val get : t -> int -> int -> float

  (** Set a specific element of a matrix. *)
  val set : t -> int -> int -> float -> unit

  (** Copy row from a matrix *)
  val row : t -> int -> vec

  (** Diagonalizes a matrix M so that M = PxDxP^T; returns (v,P) where
      v is the diagonal vector of D.*)
  val diagonalize : t -> vec * t

  val transpose : t -> t

  (** Computes the inverse of a matrix. *)
  val inverse: t -> t

  (** [zero_eigen_vector m] is a vector [v] such that [Vec.sum v = 1]
      and [mat_vec_mul m v = zero] *)
  val zero_eigen_vector : t -> vec

  val of_arrays : float array array -> t option
  val of_arrays_exn : float array array -> t

  (** Prints a matrix to the standard output (display may be messy). *)
  val pp : Format.formatter -> t -> unit
end

module type S = sig
  type vec
  type mat

  module Vector : Vector with type t = vec
  module Matrix : Matrix with type t = mat and type vec := vec
end

include S with type vec = private Owl.Mat.mat
           and type mat = private Owl.Mat.mat
