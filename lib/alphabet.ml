open Base

module type S = sig
  type t
  type 'a vector
  type 'a matrix
  val equal : t -> t -> bool
  val all : t list
  val card : int
  val to_int : t -> int
  val vector : (t -> 'a) -> 'a vector
  val vector_map : 'a vector -> f:('a -> 'b) -> 'b vector
  val reduce : 'a vector -> f:('a -> 'a -> 'a) -> 'a
  val matrix : (t -> t -> 'a) -> 'a matrix
  val ( .%() ) : 'a vector -> t -> 'a
  val ( .%()<- ) : 'a vector -> t -> 'a -> unit
  val ( .%{} ) : 'a matrix -> t * t -> 'a
  val ( .%{}<- ) : 'a matrix -> t * t -> 'a -> unit
end

module type S_int = sig
  include S with type t = private int
             and type 'a vector = private 'a array
             and type 'a matrix = private 'a array array
  val of_int : int -> t option
  val of_int_exn : int -> t
  val vector_of_array_exn : 'a array -> 'a vector
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
  let vector f = Array.init card ~f
  let vector_map = Array.map
  let matrix f =
    Array.init card ~f:(fun i -> Array.init card ~f:(f i))
  let to_int i = i
  type 'a vector = 'a array
  let vector_of_array_exn a =
    if Array.length a <> card then raise (Invalid_argument "vector_of_array_exn")
    else a
  let ( .%() ) v i = v.(i)
  let ( .%()<- ) v i x = v.(i) <- x
  let reduce xs ~f = Array.reduce_exn xs ~f
  type 'a matrix = 'a array array
  let ( .%{} ) m (i,j) = m.(i).(j)
  let ( .%{}<- ) m (i, j) x = m.(i).(j) <- x
end
