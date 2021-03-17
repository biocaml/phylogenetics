include module type of Newick_ast

val from_file : string -> (t, error) result

val from_file_exn : string -> t

val from_string : string -> (t, error) result

val from_string_exn : string -> t

val to_string : t -> string
val to_file : t -> string -> unit

val with_inner_tree :
  t ->
  f:(tree -> 'a) ->
  'a

val map_inner_tree :
  t ->
  f:(tree -> tree) ->
  t
