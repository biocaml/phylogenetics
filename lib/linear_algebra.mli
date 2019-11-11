(** This modules wraps linear algebra functions *)

(** A square matrix of floats. *)
type mat = Owl.Mat.mat

(** A vector of floats. *)
type vec = Owl.Mat.mat

module Vec : sig
  type t = vec

  (** Initialises a vector from a int->float function. *)
  val init : int -> f:(int -> float) -> vec

  (** Scalar-vector product (in-place). *)
  val inplace_scal_mul: float -> vec -> unit

  (** Scalar-vector product *)
  val scal_mul : float -> vec -> vec

  (** Scalar-vector addition. *)
  val scal_add: float -> vec -> vec

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
  val diagm : vec -> mat

  (** Matrix element-wise multiplication *)
  val mul : mat -> mat -> mat

  (** Matrix addition. *)
  val add : mat -> mat -> mat

  (** Multiplication of a matrix by a scalar. *)
  val scal_mul : float -> mat -> mat

  (** Inplace multiplication of a matrix by a scalar. *)
  val inplace_scal_mul: float -> mat -> unit

  (** Matrix multiplication *)
  val dot : mat -> mat -> mat

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

  (** Diagonalizes a matrix M so that M = PxDxP^T; returns (v,P) where
      v is the diagonal vector of D.*)
  val diagonalize : mat -> vec * mat

  (** Computes the inverse of a matrix. *)
  val inverse: mat -> mat

  (** Prints a matrix to the standard output (display may be messy). *)
  val pp : Format.formatter -> mat -> unit
end

(** Matrix-vector product. *)
val mat_vec_mul: mat -> vec -> vec
