(** Module for phylogenetic trees. *)

type index = string
and branch = float * t
and t =
  | Node of branch * branch
  | Leaf of index

val pp: Format.formatter -> t -> unit

val of_preorder: string -> t

val of_newick: string -> t

val of_newick_file: string -> t

val make_random: int -> t

val to_newick: t -> string

val to_newick_file: t -> string -> unit

val nb_branches: t -> int

val get_branch_lengths: t -> float list

val set_branch_lengths: t -> float list -> t

