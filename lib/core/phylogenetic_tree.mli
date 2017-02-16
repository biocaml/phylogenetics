(** Module for phylogenetic trees. *)

(** {6 Types} *)

(** Type for evolutionary trees: binary trees
    whose edges are labelled with lengths (floats)
    and whose leaves are labelled with sequence indexes (strings)*)
type t =
  | Node of {meta:metadata; left:branch; right:branch}
  | Leaf of {meta:metadata; index:Sigs.index}
and branch = float * t
and metadata = {id:int; routing_no:int}


(** {6 Creation/Conversion} *)

val of_preorder: string -> t

val of_newick: string -> t

val of_newick_file: string -> t

val make_random: int -> t

val to_newick: t -> string

val to_newick_file: t -> string -> unit

val to_dot: t -> string


(** {6 Parameters and transformations} *)

val nb_branches: t -> int

val get_branch_lengths: t -> float list

val set_branch_lengths: t -> float list -> t


(** {6 Constructors} *)

val build_leaf: ?routing_no:int -> Sigs.index -> t

val build_node: ?routing_no:int -> float * t -> float * t -> t

val index_of_int: int -> Sigs.index

val index_of_string: string -> Sigs.index


(** {6 Getters and setters} *)

val get_id: t -> int

val get_meta: t -> metadata

val set_meta: t -> metadata -> t

val get_routing_no: t -> int

val set_routing_no: t -> int -> t


(** {6 Comparison} *)

val equal: t -> t -> bool


(** {6 Pretty printers} *)

val pp: Format.formatter -> t -> unit

val print: t -> unit

val print_fancy: t -> unit 
