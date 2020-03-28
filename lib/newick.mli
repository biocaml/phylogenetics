include module type of Newick_ast

val from_file : string -> t
val from_string : string -> t

val to_string : t -> string
val to_file : t -> string -> unit

val map_inner_tree :
  t ->
  f:(tree -> tree) ->
  t
