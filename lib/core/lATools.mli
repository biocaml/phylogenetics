(** This modules wraps linear algebra functions (from Lacaml)
    and provide a few completely new functions (such as exponentiation)*)

(** {6 Types} *)

(** A square matrix of floats. *)
type mat

(** A vector of floats. *)
type vec



(** {6 Matrix and vector creation} *)

(** Initialises a square matrix from a int->int->float function. *)
val init_mat: int -> (int -> int -> float) -> mat

(** Initialises a vector from a int->float function. *)
val init_vec: int -> (int -> float) -> vec



(** {6 Matrix and vector operations} *)

(** Computes the product of two matrices.
    If optional argument alpha is provided then the result is also
    multiplied by scalar alpha.*)
val mult: mat -> ?alpha:float -> mat -> mat

val mat_vec_mul: mat -> vec -> vec

val vec_vec_add: vec -> vec -> vec

(** Element-wise product of two vectors. *)
val vec_vec_mul: vec -> vec -> vec

(** Sum of the elements of a vector. *)
val sum_vec_elements: vec -> float

(** Elevates a matrix to an integer power.
    If optional argument alpha is provided then the result is also
    multiplied by scalar alpha. *)
val pow: mat -> ?alpha:float -> int -> mat

(** Matrix addition. *)
val sum: mat -> mat -> mat

(** Matrix exponentiation. *)
val exp: mat -> mat

(** Multiplication of a matrix by a scalar. *)
val scal_mat_mul: mat -> float -> mat

val scal_vec_mul: vec -> float -> vec

val scal_vec_add: vec -> float -> vec

(** Element-wise logarithm of matrix *)
val log_mat: mat -> mat

(** Element-wise logarithm of vector *)
val log_vec: vec -> vec

(** Element-wise exponential of matrix*)
val unlog_vec: vec -> vec

val min_vec: vec -> float

val max_vec: vec -> float

val compare: float -> mat -> mat -> bool

val get_vec: vec -> int -> float

val get_mat: mat -> int -> int -> float

val diagonalize: mat -> mat * mat * mat

val inverse: mat -> mat

val stat_dist: mat -> vec


(** {6 Pretty printing}*)

(** Prints a matrix to the standard output (display may be messy). *)
val pp_mat: Format.formatter -> mat -> unit

(** Prints a vector to the standard output. *)
val pp_vec: Format.formatter -> vec -> unit
