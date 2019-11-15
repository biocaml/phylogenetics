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

module M = Owl.Mat

type vec = M.mat
type mat = M.mat

module Matrix = struct
  type t = mat
  let init size ~f = M.init_2d size size f
  let diagm v = M.diagm v
  let dot a b = M.dot a b

  let row mat r = M.transpose (M.row mat r)

  let inplace_scal_mul f a = M.scalar_mul_ f a

  let scal_mul f a = M.scalar_mul f a

  let add a b = M.add a b
  let mul = M.mul
  let expm a = Owl.Linalg.D.expm a

  let log m = M.log m

  let max = M.max'

  let compare ~tol:p m1 m2 =
    let diff = add m1 (scal_mul (-1.) m2) in (* substract two matrices *)
    let relative_diff = (* element-wise diff/m1 *)
      mul diff (M.map (fun x -> 1./.x) m1)
      |> M.abs
    in
    max relative_diff <= p

  let get m i j = M.get m i j
  let set m i j x = M.set m i j x

  let inverse m = M.inv m

  let apply m v = M.dot m v

  let diagonalize m =
    Owl_lapacke.syevr ~a:m ~jobz:'V' ~range:'A' ~vl:0. ~vu:0. ~il:0 ~iu:0 ~abstol:1e-6 ~uplo:'U'

  let transpose = M.transpose

  let zero_eigen_vector m =
    let n =
      let (m, n) = M.shape m in
      if m <> n then invalid_arg "Expected square matrix" else n
    in
    let a = M.(transpose (m :> mat) @= ones 1 n)
    and b = M.init_2d (n + 1) 1 (fun i _ -> if i < n then 0. else 1.) in
    Owl.Linalg.D.linsolve a b

  let of_arrays xs =
    let m = Array.length xs in
    if Array.for_all (fun row -> Array.length row = m) xs then
      Some (Owl.Mat.of_arrays xs)
    else None

  let of_arrays_exn xs =
    match of_arrays xs with
    | Some m -> m
    | None -> failwith "Arrays do not represent a matrix"

  let pp = Owl_pretty.pp_dsnda
end

module Vector = struct
  type t = vec
  let init size ~f = M.init_2d size 1 (fun i _ -> f i)
  let map v ~f = M.map f v
  let scal_add = M.scalar_add
  let scal_mul = M.scalar_mul
  let inplace_scal_mul x y = M.scalar_mul_ x y
  let add v1 v2 = M.add v1 v2
  let mul v1 v2 = M.mul v1 v2
  let sum v = M.sum' v
  let log v = M.log v
  let exp v = M.exp v
  let min v = M.min' v
  let max v = M.max' v
  let get v i = M.get v i 0
  let set v i x = M.set v i 0 x
  let pp = Owl_pretty.pp_dsnda
  let of_array xs = init (Array.length xs) ~f:(fun i -> xs.(i))
  let to_array v = M.to_array v

  let%test "Linear_algebra.Vec.{to,of}_array" =
    let xs = [| 1. ; 2. ; 3. |] in
    to_array (of_array xs) = xs
end
