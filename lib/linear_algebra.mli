(** This modules wraps linear algebra functions

    Example usage of functions in the Vector module:
    {[
      let v = Vector.init 5 ~f:(fun i -> float_of_int (i + 1)) (* Creates a vector [1.0; 2.0; 3.0; 4.0; 5.0] *)

      let v_length = Vector.length v (* Returns the length of the vector [5] *)

      let v_scaled = Vector.scal_mul 2.0 v (* Multiplies each element of the vector by 2.0 and returns the resulting vector *)

      let v_sum = Vector.sum v (* Computes the sum of the elements of the vector and returns the result [15.0] *)

      let v_get_2 = Vector.get v 2 (* Returns the element at index 2 of the vector and returns the result [3.0] *)

      let v_set_3 = Vector.set v 3 10.0 (* Sets the element at index 3 of the vector to 10.0 *)

      let v_robust_equal = Vector.robust_equal ~tol:1e-6 v v_scaled (* Compares two vectors and returns true if they are equal up to a relative difference of 1e-6 *)

      let v_array = Vector.to_array v (* Converts the vector to a float array *)

      Vector.pp Format.std_formatter v (* Prints the vector to the standard output *)

      let mapped_v = Vector.map v ~f:Float.sqrt (* Applies the square root function to each element of the vector and returns the resulting vector *)

      let logged_v = Vector.log v (* Computes the element-wise logarithm of the vector and returns the resulting vector *)

      let exponentiated_v = Vector.exp v (* Computes the element-wise exponential of the vector and returns the resulting vector *)

      let v_min = Vector.min v (* Computes the minimum element in the vector *)

      let v_max = Vector.max v (* Computes the maximum element in the vector *)
    ]}

    Example usage of functions in the Matrix module:

    {[
      let m = Matrix.init 3 ~f:(fun i j -> float_of_int (i + j)) (* Creates a matrix
                                                                    | 0.0 1.0 2.0 |
                                                                    | 1.0 2.0 3.0 |
                                                                    | 2.0 3.0 4.0 | *)

      let m_dim = Matrix.dim m (* Returns the dimensions of the matrix (3, 3) *)

      let m_transposed = Matrix.transpose m (* Transposes the matrix and returns the resulting matrix *)

      let m_inverse = Matrix.inverse m (* Computes the inverse of the matrix and returns the resulting matrix *)

      let m_row_1 = Matrix.row m 1 (* Returns the second row of the matrix as a vector *)

      let m_get_2_2 = Matrix.get m 2 2 (* Returns the element at row 2 and column 2 of the matrix *)

      let m_set_1_1 = Matrix.set m 1 1 10.0 (* Sets the element at row 1 and column 1 of the matrix to 10.0 *)

      let m_mul = Matrix.mul m m_transposed (* Computes the element-wise product of two matrices and returns the resulting matrix *)

      let m_add = Matrix.add m m_transposed (* Adds two matrices element-wise and returns the resulting matrix *)

      let m_scal_mul = Matrix.scal_mul 2.0 m (* Multiplies the matrix by a scalar and returns the resulting matrix *)

      let m_dot = Matrix.dot m m_transposed (* Computes the matrix product of two matrices and returns the resulting matrix *)

      let m_pow = Matrix.pow m 3 (* Computes the matrix raised to the power 3 and returns the resulting matrix *)

      let m_expm = Matrix.expm m (* Computes the matrix exponential and returns the resulting matrix *)

      let m_log = Matrix.log m (* Computes the element-wise logarithm of the matrix and returns the resulting matrix *)

      let m_robust_equal = Matrix.robust_equal ~tol:1e-6 m m_transposed (* Compares two matrices and returns true if they are equal up to a relative difference of 1e-6 *)

      let m_diagm = Matrix.diagm (Vector.of_array [|1.0; 2.0; 3.0|]) (* Creates a diagonal matrix from a vector *)

      Matrix.pp Format.std_formatter m (* Prints the matrix to the standard output *)
    ]}
*)

(** A vector of floats. *)
module type Vector = sig
  type t

  val length : t -> int

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

  val robust_equal : tol:float -> t -> t -> bool

  val of_array : float array -> t
  val to_array : t -> float array

  (** Prints a vector to the standard output. *)
  val pp : Format.formatter -> t -> unit
end

(** A square matrix of floats. *)
module type Matrix = sig
  type vec
  type t

  val dim : t -> int * int

  (** {5 Matrix and vector creation} *)

  (** Initialises a square matrix from a int->int->float function. *)
  val init : int -> f:(int -> int -> float) -> t

  (** [init_sym n ~f] creates a symetric square matrix by calling [f]
     only for elements s.t. [i <= j] *)
  val init_sym : int -> f:(int -> int -> float) -> t

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
  val dot :
    ?transa:[`N | `T] ->
    ?transb:[`N | `T] ->
    t -> t -> t

  (** Matrix-vector product *)
  val apply : ?trans:[`N | `T] -> t -> vec -> vec

  (** Matrix exponentiation *)
  val pow : t -> int -> t

  (** Matrix exponential *)
  val expm : t -> t

  (** Element-wise logarithm of matrix *)
  val log : t -> t

  (** Compares two matrices and tolerates a certain relative difference.
      Let f be the float parameter, it returns true iff the elements of the second matrix
      are between 1-f and 1+f times the corresponding elements of the first *)
  val robust_equal : tol:float -> t -> t -> bool

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

include S with type mat = private Lacaml.D.mat
           and type vec = private Lacaml.D.vec
