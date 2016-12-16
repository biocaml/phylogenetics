(** Module for phylogenetic trees. *)

type index = int
and branch = float * t
and t =
  | Node of branch * branch
  | Leaf of index

val pp: Format.formatter -> t -> unit

val of_preorder: string -> t

val of_newick: string -> t

val of_newick_file: string -> t
