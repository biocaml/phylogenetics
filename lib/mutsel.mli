(**
   https://www.biorxiv.org/content/biorxiv/early/2016/04/30/037689.full.pdf
   https://academic.oup.com/mbe/article/34/1/204/2656188
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
val stationary_distribution : param -> NSCodon.vector
val transition_probability_matrix : param -> float -> NSCodon.matrix
