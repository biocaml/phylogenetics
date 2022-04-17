include module type of Newick_ast

val from_file : string -> (t, [> error]) result

val from_file_exn : string -> t

val from_string : string -> (t, [> error]) result

val from_string_exn : string -> t

val to_string : t -> string
val to_file : t -> string -> unit

module Tree_repr : sig
  type ast = t

  type node_info = {
    name : string option ;
    tags : tag list ;
  }

  type tree = (node_info, node_info, float option) Tree.t
  type branch = (node_info, node_info, float option) Tree.branch

  type t =
    | Tree of tree
    | Branch of branch

  val of_ast : ast -> t

  val map_inner_tree : t -> f:(tree -> tree) -> t
  val with_inner_tree : t -> f:(tree -> 'a) -> 'a
end
