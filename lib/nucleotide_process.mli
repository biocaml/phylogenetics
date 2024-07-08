(** A representation of classical parametric processes on nucleotides

    Example usage of functions

    Example 1: Computing a rate matrix
    {[
      let example_rate_matrix =
        let model = JC69 in
        let rm = rate_matrix model in
        rm (* The computed rate matrix *)
    ]}

    Example 2: Computing the stationary distribution
    {[
      let example_stationary_distribution =
        let model = K80 2.0 in
        let sd = stationary_distribution model in
        sd (* The computed stationary distribution *)

    ]}

    Example 3: Sampling a mutation model using HKY85
    {[
      let example_hky85_model =
        let rng = Gsl.Rng.make Gsl.Rng.MT19937 in
        let alpha = 0.5 in
        let model = Random.hky85 rng ~alpha in
        model (* The sampled HKY85 mutation model *)
    ]}

    Example 4: Sampling a mutation model using GTR
    {[
      let example_gtr_model =
        let rng = Gsl.Rng.make Gsl.Rng.MT19937 in
        let alpha = 1.0 in
        let model = Random.gtr rng ~alpha in
        model (* The sampled GTR mutation model *)
    ]}

 *)

type t =
  | JC69
  | K80 of float
  | HKY85 of {
      stationary_distribution : Nucleotide.vector ;
      transition_rate : float ;
      transversion_rate : float ;
    }
  | GTR of {
      stationary_distribution : Nucleotide.vector ;
      exchangeabilities : Nucleotide.matrix ;
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
