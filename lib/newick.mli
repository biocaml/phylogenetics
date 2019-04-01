include module type of Newick_ast

val from_file : string -> t
val from_string : string -> t

val to_string : t -> string
val to_file : t -> string -> unit
