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
    include Linear_algebra.Vector with type t := vector
    val init : (t -> float) -> vector
    val map : vector -> f:(float -> float) -> vector
    val sum : vector -> float
    val normalize : vector -> vector
    val of_array : float array -> vector option
    val of_array_exn : float array -> vector
    val upcast_exn : Linear_algebra.Lacaml.vec -> vector
  end
  val flat_profile : unit -> vector
  val random_profile : float -> vector
  module Matrix : sig
    include Linear_algebra.Matrix with type t := matrix
                                   and type vec := vector
    val init : (t -> t -> float) -> matrix
    val of_arrays : float array array -> matrix option
    val of_arrays_exn : float array array -> matrix
  end
  val ( .%() ) : vector -> t -> float
  val ( .%()<- ) : vector -> t -> float -> unit
  val ( .%{} ) : matrix -> t * t -> float
  val ( .%{}<- ) :  matrix -> t * t -> float -> unit
end

module type S_int = sig
  include S with type t = private int
             and type vector = private Linear_algebra.Lacaml.vec
             and type matrix = private Linear_algebra.Lacaml.mat
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
    include Linear_algebra.Lacaml.Vector
    let init f = init card ~f
    let normalize v =
      let s = sum v in
      map v ~f:(fun x -> x /. s)
    let of_array_exn a =
      if Array.length a <> card then raise (Invalid_argument "vector_of_array_exn")
      else init (fun i -> a.(i))
    let of_array a =
      try Some (of_array_exn a)
      with _ -> None
    let upcast_exn a =
      let n = Linear_algebra.Lacaml.Vector.length a in
      if n = card
      then a
      else
        invalid_argf "vector_of_arr_exn: argument has shape %d" n ()
  end

  let flat_profile () =
    let theta = Float.(1. / of_int card) in
    Vector.init (fun _ -> theta)

  let random_profile alpha =
    let v = Owl.Stats.dirichlet_rvs ~alpha:(Array.create ~len:card alpha) in
    Vector.init (fun i -> v.(i))

  module Matrix = struct
    include Linear_algebra.Lacaml.Matrix
            
    let init f = init card ~f
    let of_arrays_exn xs =
      let m = Array.length xs in
      if m = card then of_arrays_exn xs
      else failwith "Incorrect dimension"
    let of_array xs =
      let m = Array.length xs in
      if m = card then of_arrays xs
      else None
  end

  let to_int i = i
  type vector = Linear_algebra.Lacaml.vec

  let ( .%() ) v i = Vector.get v i
  let ( .%()<- ) v i x = Vector.set v i x
  type matrix = Linear_algebra.Lacaml.mat
  let ( .%{} ) m (i,j) = Matrix.get m i j
  let ( .%{}<- ) m (i, j) x = Matrix.set m i j x
end
