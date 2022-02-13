(** A representation of classical parametric processes on nucleotides *)

type t =
  | JC69
  | K80 of float
  | HKY85 of {
      equilibrium_frequencies : Nucleotide.vector ;
      transition_rate : float ;
      transversion_rate : float ;
    }
  | GTR of {
      equilibrium_frequencies : Nucleotide.vector ;
      exchangeabilities : Linear_algebra.vec
    }

val rate_matrix : t -> Rate_matrix.Nucleotide.t

val stationary_distribution : t -> Nucleotide.vector

module Random : sig
  val hky85 : Gsl.Rng.t -> alpha:float -> t
  (** [hky85 rng ~alpha] uses [alpha] as Dirichlet parameter to
      sample a stationary profile, and draws transversion/transition
      rates from a Gamma(1, 1) *)

  val gtr : Gsl.Rng.t -> alpha:float -> t
  (** [gtr rng alpha] uses [alpha] as Dirichlet parameter to sample
      a stationary profile, and draws exchangeabilities from a
      Gamma(1, 1) *)
end
