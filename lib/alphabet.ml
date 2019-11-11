open Base

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
  end
  val flat_profile : unit -> vector
  val random_profile : float -> vector
  module Matrix : sig
    val init : (t -> t -> float) -> matrix
    val scal_mul : matrix -> float -> matrix
    val expm : matrix -> matrix
  end
  val ( .%() ) : vector -> t -> float
  val ( .%()<- ) : vector -> t -> float -> unit
  val ( .%{} ) : matrix -> t * t -> float
  val ( .%{}<- ) :  matrix -> t * t -> float -> unit
end

module type S_int = sig
  include S with type t = private int
             and type vector = private Owl.Arr.arr
             and type matrix = private Owl.Mat.mat
             and type 'a table = private 'a array
  val of_int : int -> t option
  val of_int_exn : int -> t
  val vector_of_arr_exn : Owl.Arr.arr -> vector
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
    let init f = Owl.Arr.init [|card|] f
    let map v ~f = Owl.Arr.map f v
    let sum xs = Owl.Arr.sum' xs
    let normalize v =
      let s = sum v in
      map v ~f:(fun x -> x /. s)
    let of_array_exn a =
      if Array.length a <> card then raise (Invalid_argument "vector_of_array_exn")
      else Owl.Arr.of_array a [| card |]
  end

  let flat_profile () =
    let theta = Float.(1. / of_int card) in
    Vector.init (fun _ -> theta)

  let random_profile alpha =
    [| Owl.Stats.dirichlet_rvs ~alpha:(Array.create ~len:card alpha) |]
    |> Owl.Arr.of_arrays

  module Matrix = struct
    let init f =
      Owl.Mat.init_2d card card f

    let expm = Owl.Linalg.D.expm

    let scal_mul mat tau = Owl.Mat.(mat *$ tau)
  end

  let to_int i = i
  type vector = Owl.Arr.arr

  let vector_of_arr_exn a =
    match Owl.Arr.shape a with
    | [| n |] when n = card -> a
    | _ -> raise (Invalid_argument "vector_of_array_exn")

  let ( .%() ) v i = Owl.Arr.get v [|i|]
  let ( .%()<- ) v i x = Owl.Arr.set v [| i |] x
  type matrix = Owl.Mat.mat
  let ( .%{} ) m (i,j) = Owl.Mat.get m  i j
  let ( .%{}<- ) m (i, j) x = Owl.Mat.set m i j x
end
