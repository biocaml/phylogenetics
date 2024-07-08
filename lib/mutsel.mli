(** Mutation-selection model for codons

    This module defines a parameterization for the Mutsel evolutionary
    model for codons, as well as functions to compute the
    corresponding transition rate matrix and the associated stationary
    distribution.

    References:
    @see <https://www.biorxiv.org/content/biorxiv/early/2016/04/30/037689.full.pdf>
    @see <https://academic.oup.com/mbe/article/34/1/204/2656188>
*)

module NSCodon = Codon.Universal_genetic_code.NS
module NSCodon_rate_matrix : module type of Rate_matrix.Make(NSCodon)

type param = {
  nucleotide_rates : Rate_matrix.Nucleotide.t ;
  nucleotide_stat_dist : Nucleotide.vector ;
  omega : float ; (* dN/dS *)
  scaled_fitness : Amino_acid.vector ;
  gBGC : float ;
  pps : float ; (* persistent positive selection intensity Z as in Tamuri & dos Reis 2021 *)
}

val random_param :
  Gsl.Rng.t ->
  nucleotide_process:Nucleotide_process.t ->
  alpha:float ->
  param

val flat_param : unit -> param

val rate_matrix : param -> NSCodon_rate_matrix.t
(**
   [rate_matrix param] computes the rate matrix for codon
   substitutions based on the given parameter values [param]. The
   resulting matrix is a rate matrix, meaning that its off-diagonal
   elements are positive and its lines sum to 0.

   Example:
   {[
     let param = flat_param () in
     let matrix = rate_matrix param in
   ]}
*)

val stationary_distribution : param -> NSCodon.vector
(**
   [stationary_distribution param] calculates the stationary
   distribution under parameter value [param].

   Example:
   {[
     let param = flat_param () in
     let distribution = stationary_distribution param in
   ]}
*)

val transition_probability_matrix : param -> float -> NSCodon.matrix
(**
   [transition_probability_matrix param t] computes the transition
   probability matrix for codon substitutions over a specified time
   period [t] based on the given parameter values [param]. The
   resulting matrix is a probability matrix, meaning that all elements
   are probabilities and lines sum to 1.

   Example:
   {[
     let param = flat_param () in
     let time = 0.1 in
     let transition_matrix = transition_probability_matrix param time in
   ]}
*)
