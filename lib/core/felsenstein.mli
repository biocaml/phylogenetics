(** Functions that implement Felsenstein's "pruning" algorithm to compute
    likelihood of phylogenetic trees with known sequences at leaves.*)

module Make: functor (E:Sigs.EVOL_MODEL) -> sig

  (** Single-site. felsenstein without underflow prevention. *)
  val felsenstein_single : ?shift:(float -> float -> Linear_algebra_tools.vec -> Linear_algebra_tools.vec * float) ->
    E.t -> site:int -> TopoTree.t -> E.Align.t -> float

  (** Single-site felsenstein with underflow prevention (configure threshold through threshold parameter). *)
  val felsenstein_single_shift : ?threshold:float -> E.t -> site:int -> TopoTree.t -> E.Align.t -> float

  (** Multisite felsenstein without underflow prevention. *)
  val felsenstein_noshift : E.t -> TopoTree.t -> E.Align.t -> float

  (** Multisite felsenstein with underflow prevention (use this by default). *)
  val felsenstein : E.t -> TopoTree.t -> E.Align.t -> float

end
