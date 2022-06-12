open Linear_algebra

type matrix_decomposition = [
  | `Mat of mat
  | `Transpose of mat
  | `Diag of vec
] list

val matrix_decomposition_reduce : dim:int -> matrix_decomposition -> mat

type shifted_vector = SV of vec * float

val pruning :
  ('n, 'l, 'b) Tree.t ->
  nstates:int ->
  transition_matrix:('b -> matrix_decomposition) ->
  leaf_state:('l -> int) ->
  root_frequencies:vec ->
  float
(** [pruning t ~nstates ~transition_matrix ~leaf_state
   ~root_frequencies] returns the probability of observing the states
   returned by [leaf_state] at the leaves of [t] given the CTMC
   specified by [nstates], [transition_matrix] and
   [root_frequencies]. *)

val pruning_with_missing_values :
  ('n, 'l, 'b) Tree.t ->
  nstates:int ->
  transition_matrix:('b -> matrix_decomposition) ->
  leaf_state:('l -> int option) ->
  root_frequencies:vec ->
  float
(** [pruning t ~nstates ~transition_matrix ~leaf_state
   ~root_frequencies] returns the probability of observing the states
   returned by [leaf_state] at the leaves of [t] given the CTMC
   specified by [nstates], [transition_matrix] and
   [root_frequencies]. With this variant, one can specify that some
   leaves are unobserved. *)

val pruning_with_multiple_states :
  ('a, 'b, 'c) Tree.t ->
  nstates:int ->
  transition_matrix:('c -> matrix_decomposition) ->
  leaf_state:('b -> int -> bool) ->
  root_frequencies:vec ->
  float
(** [pruning_with_multiple_states t ~nstates ~transition_matrix
   ~leaf_state ~root_frequencies] returns the probability of observing
   the states returned by [leaf_state] at the leaves of [t] given the
   CTMC specified by [nstates], [transition_matrix] and
   [root_frequencies]. With this variant one can specify some
   uncertainty for a given leaf, by letting [leaf_state] return [true]
   for several states. In that case, it is understood that each state
   has equal probability. If [leaf_state] always returns [false] for a
   given leaf, it is interpreted as the leaf being unobserved. *)

val conditionial_likelihoods :
  ('n, 'l, 'b) Tree.t ->
  nstates:int ->
  leaf_state:('l -> int) ->
  transition_matrix:('b -> matrix_decomposition) ->
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
