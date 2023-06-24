(** Phylogenetic inference by parcimony *)

val forward :
  ?cost:(int -> int -> float) ->
  n:int ->
  category:('l -> int option) ->
  ('n, 'l, 'b) Tree.t ->
  float array * ('n * int List1.t array, 'l, 'b) Tree.t

val backward :
  float array ->
  ('n * int List1.t array, 'l, 'b) Tree.t ->
  ('n * int, 'l * int, 'b) Tree.t

val fitch :
  ?cost:(int -> int -> float) ->
  n:int ->
  category:('l -> int option) ->
  ('n, 'l, 'b) Tree.t ->
  ('n * int, 'l * int, 'b) Tree.t
