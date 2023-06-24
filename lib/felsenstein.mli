(** Deprecated, see {! Phylo_ctmc}. Functions that implement Felsenstein's "pruning" algorithm to compute
    likelihood of phylogenetic trees with known sequences at leaves.*)

module type Alignment = sig
  type t
  type base
  val get_base : t -> seq:string -> pos:int -> base
  val length : t -> int
end

module Make
    (A : Alphabet.S)
    (Align : Alignment with type base := A.t)
    (E : Site_evolution_model.S with type mat := A.matrix
                                 and type vec := A.vector) :
sig
  (** Single-site. felsenstein without underflow prevention. *)
  val felsenstein_single :
    ?shift:(float -> float -> A.vector -> A.vector * float) ->
    E.param ->
    site:int ->
    Phylogenetic_tree.t ->
    Align.t ->
    float

  (** Single-site felsenstein with underflow prevention (configure threshold through threshold parameter). *)
  val felsenstein_single_shift :
    ?threshold:float ->
    E.param ->
    site:int ->
    Phylogenetic_tree.t ->
    Align.t ->
    float

  (** Multisite felsenstein without underflow prevention. *)
  val felsenstein_noshift :
    E.param ->
    Phylogenetic_tree.t ->
    Align.t ->
    float

  (** Multisite felsenstein with underflow prevention (use this by default). *)
  val felsenstein :
    E.param ->
    Phylogenetic_tree.t ->
    Align.t ->
    float
end
