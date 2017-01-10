(** Probability and statistics tools (eg, samplers and distribution handling). *)

(** {6 Samplers} *)

val sample_float_uniform: ?min:float -> float -> unit -> float

val sample_branch_lengths: branchs:(int->bool) -> sampler:(unit->float) -> TopoTree.t -> unit -> TopoTree.t
