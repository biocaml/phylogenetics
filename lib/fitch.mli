module Cost : sig
  type t = Int of int | Infinity
  val zero : t
  val compare : t -> t -> int
  val ( + ) : t -> t -> t
end

val forward :
  n:int ->
  (_, int, 'b) Tree.t ->
  Cost.t array * (int Non_empty_list.t array, int, 'b) Tree.t

val backward :
  Cost.t array ->
  (int Non_empty_list.t array, int, 'b) Tree.t ->
  (int, int, 'b) Tree.t

val fitch :
  n:int ->
  (_, int, 'b) Tree.t ->
  (int, int, 'b) Tree.t
  
