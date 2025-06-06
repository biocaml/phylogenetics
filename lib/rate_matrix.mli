(** Continuous Time Markov Chain rate matrix

    A rate matrix is the
    {{:https://en.wikipedia.org/wiki/Infinitesimal_generator_(stochastic_processes)}infinitesimal
    generator} for a discrete-space continuous time markov process. It
    is basically a matrix such that all off-diagonal elements are
    positive and each diagonal element is minus the sum of all other
    elements in the row.  *)


module type S = sig
  type vector
  type matrix
  type symbol
  type t = matrix

  val make : (symbol -> symbol -> float) -> t
  (** [make f] is a matrix such that [f i j] is the rate of transition
      from state [i] to state [j]. [f] is called only for [i <> j]. *)

  val make_symetric : (symbol -> symbol -> float) -> t

  val jc69 : unit -> t
  (** {{:https://en.wikipedia.org/wiki/Models_of_DNA_evolution#JC69_model_(Jukes_and_Cantor_1969)}Jukes and Cantor 1969} *)

  val gtr :
    stationary_distribution:vector ->
    exchangeabilities:matrix ->
    t
  (**
     {{:https://en.wikipedia.org/wiki/Models_of_DNA_evolution#GTR_model_(Tavar%C3%A9_1986)}Generalised
     Time-Reversible model}. [exchangeabilities] should be a symetric
     matrix with arbitrary diagonal *)

  val stationary_distribution : t -> vector
  (** [stationary_distribution r] numerically computes the asymptotic
      probability distribution [pi] of the CTMC defined by [r]. *)

  val scaled_rate_matrix : vector -> t -> t
  (** [scaled_rate_matrix pi r] is a new matrix rate such that the
      corresponding CTMC has one expected transition per unit of time
      under the distribution [pi]. In addition, if [r] is symetrical,
      the result has [pi] as stationary distribution. *)

  val scale : t -> t
  (** rescale matrix such that the sum of off-diagonal elements is 1. *)
end

module Make(A : Alphabet.S_int) : S with type symbol := A.t
                                     and type vector := A.vector
                                     and type matrix := A.matrix


module Nucleotide : sig
  include module type of Make(Nucleotide)
  val k80 : float -> t
  val hky85 :
    stationary_distribution:Nucleotide.vector ->
    transition_rate:float ->
    transversion_rate:float ->
    t
end

module Amino_acid : sig
  include module type of Make(Amino_acid)
end

val make : int -> f:(int -> int -> float) -> Linear_algebra.mat
(** [make n f] is a [n] x [n] matrix such that [f i j] is the rate of
    transition from state [i] to state [j]. [f] is only called for [i
    <> j]. *)

val transition_probability_matrix :
  tau:float ->
  rates:float array array ->
  float array array
