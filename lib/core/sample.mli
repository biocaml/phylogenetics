(** {6 Floats} *)
val sample_float_uniform: ?min:float -> float -> unit -> float

(** {6 Trees} *)
val sample_branch_lengths: branchs:(int->bool) -> sampler:(unit->float) -> TopoTree.t -> unit -> TopoTree.t
