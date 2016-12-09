(** This modules wraps linear algebra functions (from Lacaml)
    and provide a few completely new functions (such as exponentiation)*)

(** {6 Types} *)
(** A square matrix of floats. *)
type mat

(** A vector of floats. *)
type vec



(** {6 Matrix creation} *)
(** Initialises a square matrix from a int->int->float function. *)
val init_mat: int -> (int -> int -> float) -> mat

(** Initialises a vector from a int->float function. *)
val init_vec: int -> (int -> float) -> vec



(** {6 Matrix and vector operations} *)
(** Computes the product of two matrices.
    If optional argument alpha is provided then the result is also
    multiplied by scalar alpha.*)
val mult: mat -> ?alpha:float -> mat -> mat

(** TO DO *)
val mat_vec_mul: mat -> vec -> vec

(** TO DO *)
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
val scal_mat_mult: mat -> float -> mat



(** {6 Printing and tests}*)
(** Prints a matrix to the standard output (display may be messy). *)
val pp_mat: Format.formatter -> mat -> unit

(** Prints a vector to the standard output. *)
val pp_vec: Format.formatter -> vec -> unit
