(** Functions that implement Felsenstein's "pruning" algorithm to compute
    likelihood of phylogenetic trees with known sequences at leaves.*)

open Linear_algebra_tools

module type Base = sig
  type t
end

module type Alignment = sig
  type t
  type base
  val get_base : t -> seq:string -> pos:int -> base
  val length : t -> int
end

module type Evol_model = sig
  type t
  type base
  val eMt_mat : t -> float -> mat
  val known_vector : base -> vec
  val stat_dist_vec : t -> vec
end

module Make(Base : Base)(Align : Alignment with type base := Base.t)(E : Evol_model with type base := Base.t) : sig

  (** Single-site. felsenstein without underflow prevention. *)
  val felsenstein_single : ?shift:(float -> float -> Linear_algebra_tools.vec -> Linear_algebra_tools.vec * float) ->
    E.t -> site:int -> Phylogenetic_tree.t -> Align.t -> float

  (** Single-site felsenstein with underflow prevention (configure threshold through threshold parameter). *)
  val felsenstein_single_shift : ?threshold:float -> E.t -> site:int -> Phylogenetic_tree.t -> Align.t -> float

  (** Multisite felsenstein without underflow prevention. *)
  val felsenstein_noshift : E.t -> Phylogenetic_tree.t -> Align.t -> float

  (** Multisite felsenstein with underflow prevention (use this by default). *)
  val felsenstein : E.t -> Phylogenetic_tree.t -> Align.t -> float

end
