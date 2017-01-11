(** Module for phylogenetic trees. *)

(** {6 Types} *)

type index = string
and branch = float * t

(** Type for evolutionary trees: binary trees
    whose edges are labelled with lengths (floats)
    and whose leaves are labelled with sequence indexes (strings)*)
and t =
  | Node of branch * branch
  | Leaf of index


(** {6 Creation/Conversion} *)

val of_preorder: string -> t

val of_newick: string -> t

val of_newick_file: string -> t

val make_random: int -> t

val to_newick: t -> string

val to_newick_file: t -> string -> unit


(** {6 Parameters and transformations} *)

val nb_branches: t -> int

val get_branch_lengths: t -> float list

val set_branch_lengths: t -> float list -> t

val reroot: t -> int -> t


(** {6 Pretty printers} *)

val pp: Format.formatter -> t -> unit
