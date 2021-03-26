open Linear_algebra.Lacaml

type shifted_vector = SV of vec * float

val pruning :
  ('n, 'l, 'b) Tree.t ->
  nstates:int ->
  transition_matrix:('b -> mat) ->
  leaf_state:('l -> int) ->
  root_frequencies:vec ->
  float

val pruning_with_missing_values :
  ('n, 'l, 'b) Tree.t ->
  nstates:int ->
  transition_matrix:('b -> mat) ->
  leaf_state:('l -> int option) ->
  root_frequencies:vec ->
  float

val conditionial_likelihoods :
  ('n, 'l, 'b) Tree.t ->
  nstates:int ->
  transition_matrix:('b -> mat) ->
  leaf_state:('l -> int) ->
  (shifted_vector, shifted_vector, mat) Tree.t

val conditional_simulation :
  (shifted_vector, shifted_vector, mat) Tree.t ->
  root_frequencies:vec ->
  choose:(vec -> int) ->
  (int, int, mat) Tree.t
