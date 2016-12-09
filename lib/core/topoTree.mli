(** Module for phylogenetic trees. *)

type index = int
and branch = float * t
and t =
  | Node of branch * branch
  | Leaf of index

val pp: Format.formatter -> t -> unit

val of_string: string -> t
