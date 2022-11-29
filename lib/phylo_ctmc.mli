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
  transition_probabilities:('b -> matrix_decomposition) ->
  leaf_state:('l -> int) ->
  root_frequencies:vec ->
  float
(** [pruning t ~nstates ~transition_probabilities ~leaf_state
   ~root_frequencies] returns the probability of observing the states
   returned by [leaf_state] at the leaves of [t] given the CTMC
   specified by [nstates], [transition_probabilities] and
   [root_frequencies]. *)

val pruning_with_missing_values :
  ('n, 'l, 'b) Tree.t ->
  nstates:int ->
  transition_probabilities:('b -> matrix_decomposition) ->
  leaf_state:('l -> int option) ->
  root_frequencies:vec ->
  float
(** [pruning t ~nstates ~transition_probabilities ~leaf_state
   ~root_frequencies] returns the probability of observing the states
   returned by [leaf_state] at the leaves of [t] given the CTMC
   specified by [nstates], [transition_probabilities] and
   [root_frequencies]. With this variant, one can specify that some
   leaves are unobserved. *)

val conditional_likelihoods :
  ('n, 'l, 'b) Tree.t ->
  nstates:int ->
  leaf_state:('l -> int) ->
  transition_probabilities:('b -> mat) ->
  (shifted_vector, int, 'b * mat) Tree.t

val conditional_simulation :
  Gsl.Rng.t ->
  (shifted_vector, int, 'b * mat) Tree.t ->
  root_frequencies:vec ->
  (int, int, 'b * mat) Tree.t

(** In this variant implementation, one can specify some uncertainty
   for a given leaf, by letting [leaf_state] return [true] for several
   states. In that case, it is understood that each state has equal
   probability. If [leaf_state] always returns [false] for a given
   leaf, it is interpreted as the leaf being unobserved. *)
module Ambiguous : sig
  val pruning :
    ('a, 'b, 'c) Tree.t ->
    nstates:int ->
    transition_probabilities:('c -> matrix_decomposition) ->
    leaf_state:('b -> int -> bool) ->
    root_frequencies:vec ->
    float
  (** [pruning t ~nstates ~transition_probabilities ~leaf_state
     ~root_frequencies] returns the probability of observing the
     states returned by [leaf_state] at the leaves of [t] given the
     CTMC specified by [nstates], [transition_probabilities] and
     [root_frequencies]. *)

  val conditional_likelihoods :
    ('n, 'l, 'b) Tree.t ->
    nstates:int ->
    leaf_state:('l -> int -> bool) ->
    transition_probabilities:('b -> mat) ->
    (shifted_vector, int array, 'b * mat) Tree.t

  val conditional_simulation :
    Gsl.Rng.t ->
    (shifted_vector, int array, 'b * mat) Tree.t ->
    root_frequencies:vec ->
    (int, int, 'b * mat) Tree.t
end

module Uniformized_process : sig
  type t
  val make :
    transition_rates:mat ->
    transition_probabilities:(float -> mat) ->
    (branch_length:float -> t) Core.Staged.t

  val transition_rates : t -> mat
  val transition_probabilities : t -> mat
end

module Path_sampler : sig
  type t
  val uniformization : Uniformized_process.t -> t

  val rejection_sampling :
    max_tries:int ->
    rates:mat ->
    branch_length:float ->
    unit -> t

  val rejection_sampling_or_uniformization :
    max_tries:int ->
    Uniformized_process.t ->
    t

  val sample_exn :
    t ->
    rng:Gsl.Rng.t ->
    start_state:int ->
    end_state:int ->
    (int * float) array
end

val substitution_mapping :
  rng:Gsl.Rng.t ->
  path_sampler:('b -> Path_sampler.t) ->
  (int, int, 'b * mat) Tree.t ->
  (int, int, 'b * (int * float) array) Tree.t
