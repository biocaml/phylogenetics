(** Probability and statistics tools (eg, samplers and distribution handling). *)

(** {5 Samplers} *)

val sample_branch_lengths: branchs:(int->bool) -> sampler:(unit->float) -> Phylogenetic_tree.t -> unit -> Phylogenetic_tree.t


(** {5 Distributions} *)

type sample_list = float list

val sample_list_of_file: string -> sample_list

val sample_list_extrema: sample_list -> float * float

val sample_list_mean: sample_list -> float
