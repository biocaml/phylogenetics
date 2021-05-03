module type Branch_info = sig
  type t
  val length : t -> float
end

module Make
    (A : Alphabet.S_int)
    (BI : Branch_info) :
sig
  val transition_matrix :
    (BI.t -> A.matrix) ->
    BI.t ->
    A.matrix

  val site_exponential_method :
    Gsl.Rng.t ->
    (_, _, BI.t) Tree.t ->
    root:A.t ->
    transition_matrix:(BI.t -> A.matrix) ->
    (A.t, A.t, BI.t) Tree.t

  val site_gillespie_direct :
    Gsl.Rng.t ->
    (_, _, BI.t) Tree.t ->
    root:A.t ->
    rate_matrix:(BI.t -> A.matrix) ->
    (A.t, A.t, BI.t) Tree.t

  val site_gillespie_first_reaction :
    Gsl.Rng.t ->
    (_, _, BI.t) Tree.t ->
    root:A.t ->
    rate_matrix:(BI.t -> A.matrix) ->
    (A.t, A.t, BI.t) Tree.t

  val sequence_gillespie_direct :
    Gsl.Rng.t ->
    (_, _, BI.t) Tree.t ->
    update_iterator:(n:int -> pos:int -> (int -> unit) -> unit) ->
    root:A.t array ->
    rate_vector:(BI.t -> A.t array -> int -> float A.table) ->
    (A.t array, A.t array, BI.t) Tree.t

  val hmm0 :
    Gsl.Rng.t ->
    len:int ->
    dist:(int -> float A.table) ->
    A.t array
end

module NSCodon(BI : Branch_info) : sig
  include module type of Make(Mutsel.NSCodon)(BI)

  val alignment :
    Gsl.Rng.t ->
    (_, _, BI.t) Tree.t ->
    root:Mutsel.NSCodon.t array ->
    rate_matrix:(int -> BI.t -> Mutsel.NSCodon_rate_matrix.t) ->
    Dna.t list
end
