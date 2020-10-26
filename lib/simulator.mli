module type Evolution_model = sig
  type param
  type vector
  type matrix
  val stationary_distribution : param -> vector
  val rate_matrix : param -> matrix
end

module type Branch_info = sig
  type t
  val length : t -> float
end

module Make
    (A : Alphabet.S_int)
    (M : Evolution_model with type vector := A.vector
                          and type matrix := A.matrix)
    (BI : Branch_info) :
sig
  val site_exponential_method :
    (_, _, BI.t) Tree.t ->
    root:A.t ->
    param:(BI.t -> M.param) ->
    (A.t, A.t, BI.t) Tree.t

  val site_gillespie_direct :
    (_, _, BI.t) Tree.t ->
    root:A.t ->
    param:(BI.t -> M.param) ->
    (A.t, A.t, BI.t) Tree.t

  val site_gillespie_first_reaction :
    (_, _, BI.t) Tree.t ->
    root:A.t ->
    param:(BI.t -> M.param) ->
    (A.t, A.t, BI.t) Tree.t

  val sequence_gillespie_direct :
    (_, _, BI.t) Tree.t ->
    root:A.t array ->
    param:(A.t array -> int -> BI.t -> M.param) ->
    (A.t array, A.t array, BI.t) Tree.t

  val hmm0 :
    len:int ->
    dist:(int -> float A.table) ->
    A.t array
end

module Mutsel(BI : Branch_info) : sig
  include module type of Make(Mutsel.NSCodon)(Mutsel)(BI)

  val alignment :
    (_, _, BI.t) Tree.t ->
    root:Mutsel.NSCodon.t array ->
    (int -> BI.t -> Mutsel.param) ->
    Dna.t list
end
