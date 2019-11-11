open Core_kernel

module type S = sig
  type t
  type vector
  type matrix
  type 'a table
  val equal : t -> t -> bool
  val all : t list
  val card : int
  val to_int : t -> int
  module Table : sig
    val init : (t -> 'a) -> 'a table
    val map : 'a table -> f:('a -> 'b) -> 'b table
    val of_array_exn : 'a array -> 'a table
  end
  module Vector : sig
    val init : (t -> float) -> vector
    val map : vector -> f:(float -> float) -> vector
    val sum : vector -> float
    val normalize : vector -> vector
    val of_array_exn : float array -> vector
    val upcast_exn : Linear_algebra.vec -> vector
  end
  val flat_profile : unit -> vector
  val random_profile : float -> vector
  module Matrix : sig
    val init : (t -> t -> float) -> matrix
    val scal_mul : float -> matrix -> matrix
    val expm : matrix -> matrix
  end
  val ( .%() ) : vector -> t -> float
  val ( .%()<- ) : vector -> t -> float -> unit
  val ( .%{} ) : matrix -> t * t -> float
  val ( .%{}<- ) :  matrix -> t * t -> float -> unit
end

module type S_int = sig
  include S with type t = private int
             and type vector = private Linear_algebra.vec
             and type matrix = private Linear_algebra.mat
             and type 'a table = private 'a array
  val of_int : int -> t option
  val of_int_exn : int -> t
end

module Make(X : sig val card : int end) = struct
  type t = int
  include X
  let of_int i =
    if i < 0 || i >= card then None
    else Some i

  let of_int_exn n =
    if n < 0 || n >= card then raise (Invalid_argument "of_int_exn")
    else n

  let equal = Int.( = )
  let all = List.init card ~f:Fn.id
  type 'a table = 'a array
  module Table = struct
    let init f = Array.init card ~f
    let map = Array.map
    let of_array_exn a =
      if Array.length a <> card then raise (Invalid_argument "vector_of_array_exn")
      else a
  end
  module Vector = struct
    let init f = Linear_algebra.Vec.init card ~f
    let map v ~f = Linear_algebra.Vec.map v ~f
    let sum xs = Linear_algebra.Vec.sum xs
    let normalize v =
      let s = sum v in
      map v ~f:(fun x -> x /. s)
    let of_array_exn a =
      if Array.length a <> card then raise (Invalid_argument "vector_of_array_exn")
      else Linear_algebra.Vec.init card ~f:(fun i -> a.(i))
    let upcast_exn (a : Linear_algebra.vec) =
      match Owl.Arr.shape (a :> Owl.Mat.mat) with
      | [| n ; 1 |] when n = card -> a
      | a ->
        let shape = Sexp.to_string_hum ([%sexp_of: int array] a) in
        invalid_argf "vector_of_arr_exn: argument has shape %s" shape ()
  end

  let flat_profile () =
    let theta = Float.(1. / of_int card) in
    Vector.init (fun _ -> theta)

  let random_profile alpha =
    let v = Owl.Stats.dirichlet_rvs ~alpha:(Array.create ~len:card alpha) in
    Vector.init (fun i -> v.(i))

  module Matrix = struct
    let init f =
      Linear_algebra.Mat.init card ~f

    let expm = Linear_algebra.Mat.expm

    let scal_mul = Linear_algebra.Mat.scal_mul
  end

  let to_int i = i
  type vector = Linear_algebra.vec

  let ( .%() ) v i = Linear_algebra.Vec.get v i
  let ( .%()<- ) v i x = Linear_algebra.Vec.set v i x
  type matrix = Linear_algebra.mat
  let ( .%{} ) m (i,j) = Linear_algebra.Mat.get m i j
  let ( .%{}<- ) m (i, j) x = Linear_algebra.Mat.set m i j x
end
