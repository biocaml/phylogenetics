type index = int
and branch = float * tree
and tree =
  | Node of branch * branch
  | Leaf of index

val pretty_print: tree -> unit

val tree_of_string: string -> tree

val test: unit -> unit
