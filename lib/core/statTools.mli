(** Probability and statistics tools (eg, samplers and distribution handling). *)

(** {6 Samplers} *)

val sample_float_uniform: ?min:float -> float -> unit -> float

val sample_branch_lengths: branchs:(int->bool) -> sampler:(unit->float) -> TopoTree.t -> unit -> TopoTree.t

(** {6 Distributions} *)

type distrib

val distrib_of_file: string -> distrib

val distrib_extrema: distrib -> float * float

(** {6 Plots} *)

val bins: ?nb:int -> distrib -> float list

val plot_distrib: ?nb:int -> distrib -> unit

val plot_distribs: ?nb:int -> distrib list -> unit
