(** Likelihood calculations for continuous-time Markov chain along a tree

    This module implements several variants of the so-called pruning
    algorithm, as well as functions to sample substitution mappings
    conditionally on leaf values. *)

open Linear_algebra

type matrix_decomposition = [
  | `Mat of mat
  | `Transpose of mat
  | `Diag of vec
] list
(** The type [matrix_decomposition] represents a product of
    matrices. In the case that the product is eventually applied to a
    vector, it is faster to perform a series of matrix-vector products
    than computing the product of matrices.

    - The constructor [`Mat of mat] represents a matrix without any decomposition.
    - The constructor [`Transpose of mat] represents the transpose of a matrix.
    - The constructor [`Diag of vec] represents a diagonal matrix.

    Example usage:
    {[
      let decomposition = [
        `Mat matrix1;
        `Transpose matrix2;
        `Diag vector1;
      ] in

      let result = matrix_decomposition_reduce ~dim:3 decomposition in
      (* process_result result ...*)
    ]}
*)

val matrix_decomposition_reduce : dim:int -> matrix_decomposition -> mat
(** [matrix_decomposition_reduce dim md] computes the product of
    elements in [md] as a single matrix of dimension [dim]. *)

type shifted_vector = SV of vec * float
(** 〚SV (v, carry)〛= 〚v〛. exp(〚carry〛) *)

module SV : sig
  type t = shifted_vector

  val of_vec : vec -> t
  val shift : ?threshold:float -> vec -> carry:float -> t
  val decomp_vec_mul : matrix_decomposition -> t -> t
  val mat_vec_mul : mat -> t -> t
  val scal_vec_mul : float -> t -> t
  val mul : t -> t -> t
  val add : t -> t -> t
end

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

val conditional_likelihoods :
  ('n, 'l, 'b) Tree.t ->
  nstates:int ->
  leaf_state:('l -> int) ->
  transition_probabilities:('b -> mat) ->
  (shifted_vector, int, 'b * mat) Tree.t
(** [conditional_likelihoods t ~nstates ~leaf_state
    ~transition_probabilities] computes the conditional likelihoods of
    observing the states returned by [leaf_state] at the leaves of [t]
    given the CTMC specified by [nstates] and
    [transition_probabilities]. *)

val conditional_simulation :
  Gsl.Rng.t ->
  (shifted_vector, int, 'b * mat) Tree.t ->
  root_frequencies:vec ->
  (int, int, 'b * mat) Tree.t
(** [conditional_simulation rng tree ~root_frequencies] performs a
    conditional simulation on the provided [tree] given the
    [root_frequencies] using the provided random number generator
    [rng]. *)

(** In this variant implementation, leaves may be unobserved *)
module Missing_values : sig
  val pruning :
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
    leaf_state:('l -> int option) ->
    transition_probabilities:('b -> mat) ->
    (shifted_vector option, int option, 'b * mat) Tree.t
  (** [conditional_likelihoods t ~nstates ~leaf_state
      ~transition_probabilities] computes the conditional likelihoods of
      observing the states returned by [leaf_state] at the leaves of [t]
      given the CTMC specified by [nstates] and
      [transition_probabilities]. *)

  val conditional_simulation :
    Gsl.Rng.t ->
    (shifted_vector option, int option, 'b * mat) Tree.t ->
    root_frequencies:vec ->
    (int, int, 'b * mat) Tree.t
    (** [conditional_simulation rng tree ~root_frequencies] performs a
        conditional simulation on the provided [tree] given the
        [root_frequencies] using the provided random number generator
        [rng]. *)
end

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
  val uniformization :
    max_path_length:int ->
    Uniformized_process.t ->
    t

  val rejection_sampling :
    max_tries:int ->
    rates:mat ->
    branch_length:float ->
    unit -> t

  val rejection_sampling_or_uniformization :
    max_tries:int ->
    max_path_length:int ->
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
