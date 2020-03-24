module Cost : sig
  type t = Int of int | Infinity
  val zero : t
  val compare : t -> t -> int
  val ( + ) : t -> t -> t
end

val forward :
  n:int ->
  category:('l -> int option) ->
  ('n, 'l, 'b) Tree.t ->
  Cost.t array * ('n * int List1.t array, 'l, 'b) Tree.t

val backward :
  Cost.t array ->
  ('n * int List1.t array, 'l, 'b) Tree.t ->
  ('n * int, 'l * int, 'b) Tree.t

val fitch :
  n:int ->
  category:('l -> int option) ->
  ('n, 'l, 'b) Tree.t ->
  ('n * int, 'l * int, 'b) Tree.t
  
