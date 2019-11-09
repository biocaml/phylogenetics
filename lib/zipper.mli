(** Zipper type for fast operations on large phylogenetic trees. *)

(** {5 Types} *)

type t

type branch = float * Phylogenetic_tree.t

type direction =
  | Dir0
  | Dir1
  | Dir2

type location_type =
  | LocLeaf
  | LocBranch
  | LocNode

type oriented_zipper


(** {5 Creation / conversion} *)

val of_tree: Phylogenetic_tree.t -> t

val of_tree_dir: Phylogenetic_tree.t -> oriented_zipper

val to_tree: t -> Phylogenetic_tree.t

val orient: t -> direction -> oriented_zipper

val unorient: oriented_zipper -> t


(** {5 Constructors / object manipulation} *)

val string_of_dir: direction -> string

val dir_of_string: string -> direction


(** {5 Observation} *)

val equal: t -> t -> bool

val get_branch: t -> direction -> branch

val get_index: t -> Sigs.index

val get_length: t -> direction -> float

val get_tree: t -> direction -> Phylogenetic_tree.t

val length_left: oriented_zipper -> float

val length_right: oriented_zipper -> float

val location: t -> location_type


(** {5 Movement} *)

val goto: t -> int -> t

val init_routing: t -> t

val left: oriented_zipper -> direction

val move: t -> direction -> t

val move_left: oriented_zipper -> oriented_zipper

val move_right: oriented_zipper -> oriented_zipper

val random_node: t -> t

val right: oriented_zipper -> direction

val slide: t -> direction -> float -> t


(** {5 Pretty printing} *)

val pp: Format.formatter -> t -> unit

val pp_fancy: Format.formatter -> t -> unit

val print: t -> unit

val print_fancy: t -> unit
