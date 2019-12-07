module type Alphabet = sig
  type t
  val card : int
  val to_int : t -> int
end

module Make(A : Alphabet) : sig
  open Linear_algebra.Owl

  type shifted_vector = SV of vec * float

  val pruning :
    ('n, 'l, 'b) Tree.t ->
    transition_matrix:('b -> mat) ->
    leaf_state:('l -> A.t) ->
    root_frequencies:vec ->
    float

  val conditionial_likelihoods :
    ('n, 'l, 'b) Tree.t ->
    transition_matrix:('b -> mat) ->
    leaf_state:('l -> A.t) ->
    (shifted_vector, shifted_vector, mat) Tree.t

  val conditional_simulation :
    (shifted_vector, shifted_vector, mat) Tree.t ->
    root_frequencies:vec ->
    choose:(vec -> int) ->
    (int, int, mat) Tree.t
end
