type branch = float * TopoTree.t

type t

type direction =
  | 	Dir0
  | 	Dir1
  | 	Dir2

type location_type =
  | 	LocLeaf
  | 	LocBranch
  | 	LocNode

type oriented_zipper = {dir:direction; zipper:t}

val string_of_dir : direction -> string

val dir_of_string : string -> direction

val location : t -> location_type

val left : oriented_zipper -> direction

val right : oriented_zipper -> direction

val slide : t -> direction -> float -> t

val move : t -> direction -> t

val move_left : oriented_zipper -> oriented_zipper

val move_right : oriented_zipper -> oriented_zipper

val orient : t -> direction -> oriented_zipper

val init_routing : t -> t

val goto : t -> int -> t

val get_length : t -> direction -> float

val length_left : oriented_zipper -> float

val length_right : oriented_zipper -> float

val get_index : t -> Sigs.index

val of_tree : TopoTree.t -> t

val of_tree_dir : TopoTree.t -> oriented_zipper

val to_tree : t -> TopoTree.t

val branch : t -> direction -> branch

val pp : Format.formatter -> t -> unit

val print : t -> unit
