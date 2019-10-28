(** Probability and statistics tools (eg, samplers and distribution handling). *)

(** {6 Samplers} *)

val sample_branch_lengths: branchs:(int->bool) -> sampler:(unit->float) -> Phylogenetic_tree.t -> unit -> Phylogenetic_tree.t


(** {6 Distributions} *)

type sample_list = float list

val sample_list_of_file: string -> sample_list

val sample_list_extrema: sample_list -> float * float

val sample_list_mean: sample_list -> float


(** {6 Plots} *)

val bins: ?nb:int -> sample_list -> (float * float) list

val plot_sample_list: ?nb:int -> sample_list -> unit

val plot_sample_lists: ?nb:int -> sample_list list -> unit

val pause: unit -> unit
