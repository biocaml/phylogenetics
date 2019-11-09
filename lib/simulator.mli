module type Evolution_model = sig
  type param
  type 'a vector
  type 'a matrix
  type rate_matrix = private float matrix
  val stationary_distribution : param -> float vector
  val rate_matrix : param -> rate_matrix
end

module Make
    (A : Alphabet.S_int)
    (M : Evolution_model with type 'a vector := 'a A.vector
                          and type 'a matrix := 'a A.matrix) :
sig
  val site_exponential_method :
    (_, _, float * 'b) Tree.t ->
    root:A.t ->
    param:('b -> M.param) ->
    (A.t, A.t, float * 'b) Tree.t

  val site_gillespie_direct :
    (_, _, float * 'b) Tree.t ->
    root:A.t ->
    param:('b -> M.param) ->
    (A.t, A.t, float * 'b) Tree.t

  val site_gillespie_first_reaction :
    (_, _, float * 'b) Tree.t ->
    root:A.t ->
    param:('b -> M.param) ->
    (A.t, A.t, float * 'b) Tree.t

  val sequence_gillespie_direct :
    (_, _, float * 'b) Tree.t ->
    root:A.t array ->
    param:(A.t array -> int -> 'b -> M.param) ->
    (A.t array, A.t array, float * 'b) Tree.t

  val hmm0 :
    len:int ->
    dist:(int -> float A.vector) ->
    A.t array
end

module Mutsel : sig
  include module type of Make(Mutsel.NSCodon)(Mutsel)

  val alignment :
    (_, _, float * 'b) Tree.t ->
    root:Mutsel.NSCodon.t array ->
    (int -> 'b -> Mutsel.param) ->
    Dna.t list
end
