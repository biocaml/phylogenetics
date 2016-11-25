(** This modules wraps linear algebra functions (from Lacaml)
    and provide a few completely new functions (such as exponentiation)*)

(** {6 Types} *)
(** A square matrix of floats. *)
type mat

(** A vector of floats. *)
type vec



(** {6 Matrix creation} *)
(** A matrix with random contents for test purposes. Call to generate matrix. *)
val testMat: mat

(** The identity matrix (intended for tests). Call to generate matrix. *)
val testId: unit -> mat

(** Initialises a square matrix from a int->int->float function. *)
val init: int -> (int -> int -> float) -> mat

(** Initialises a vector from a int->float function. *)
val initvec: int -> (int -> float) -> vec



(** {6 Matrix and vector operations} *)
(** Computes the product of two matrices.
    If optional argument alpha is provided then the result is also
    multiplied by scalar alpha.*)
val mult: mat -> ?alpha:float -> mat -> mat

(** TO DO *)
val mat_vec_mult: mat -> vec -> vec

(** TO DO *)
val vec_vec_add: vec -> vec -> vec

(** TO DO *)
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
val scalmul: mat -> float -> mat



(** {6 Printing and tests}*)
(** Prints a matrix to the standard output (display may be messy). *)
val printMat: mat -> unit

(** Prints a vector to the standard output. *)
val printVec: vec -> unit

(** Functions which performs various tests
    and displays results to standard output *)
val test: unit -> unit

(** Tests the exponentiation function specifically. *)
val exptest: unit -> unit
