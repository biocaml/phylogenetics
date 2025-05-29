(** New Hampshire tree (a.k.a. newick) format parsing

    This module provides parsing utilities for Newick tree format [1] and
    its extension with tags [2].

    [1] https://phylipweb.github.io/phylip/newicktree.html
    [2] http://www.phylosoft.org/NHX/nhx.pdf
*)
include module type of Newick_ast

val from_file : string -> (t, [> error]) result

val from_file_exn : string -> t

val from_string : string -> (t, [> error]) result

val from_string_exn : string -> t

val of_tree :
  ?node_id:('a -> string option) ->
  ?node_tags:('a -> tag list) ->
  ?leaf_id:('b -> string option) ->
  ?leaf_tags:('b -> tag list) ->
  ?branch_length:('c -> float option) ->
  ?parent_branch:float ->
  ('a, 'b, 'c) Tree.t ->
  t

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
  val to_ast : t -> ast

  val map_inner_tree : t -> f:(tree -> tree) -> t
  val with_inner_tree : t -> f:(tree -> 'a) -> 'a
end
