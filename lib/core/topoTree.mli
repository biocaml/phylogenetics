type index = int
and branch = float * t
and t =
  | Node of branch * branch
  | Leaf of index

val pp: Format.formatter -> t -> unit

val tree_of_string: string -> t

val test: unit -> unit
