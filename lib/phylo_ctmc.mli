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
  leaf_state:('l -> int) ->
  transition_matrix:('b -> mat) ->
  (shifted_vector, int, 'b * mat) Tree.t

val conditional_simulation :
  Gsl.Rng.t ->
  (shifted_vector, int, 'b * mat) Tree.t ->
  root_frequencies:vec ->
  (int, int, 'b * mat) Tree.t

type uniformized_process
val uniformized_process : mat -> uniformized_process

val conditional_simulation_along_branch :
  Gsl.Rng.t ->
  uniformized_process ->
  branch_length:float ->
  start_state:int ->
  end_state:int ->
  nstates:int ->
  (int * float) array

val substitution_mapping :
  nstates:int ->
  branch_length:('b -> float) ->
  rng:Gsl.Rng.t ->
  process:('b -> uniformized_process) ->
  (int, int, 'b * mat) Tree.t ->
  (int, int, 'b * (int * float) array) Tree.t
