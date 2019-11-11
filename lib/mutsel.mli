(**
   https://www.biorxiv.org/content/biorxiv/early/2016/04/30/037689.full.pdf
   https://academic.oup.com/mbe/article/34/1/204/2656188
*)

module NSCodon = Codon.Universal_genetic_code.NS
module Codon_rate : module type of Rate_matrix.Make(NSCodon)

type rate_matrix = Codon_rate.t

type param = {
  nucleotide_rates : Rate_matrix.Nucleotide.t ;
  nucleotide_stat_dist : Nucleotide.vector ;
  omega : float ; (* dN/dS *)
  scaled_fitness : Amino_acid.vector ;
  gBGC : float ;
}
(**

*)

val random_param :
  alpha_nucleotide:float ->
  alpha_fitness:float ->
  param

val rate_matrix : param -> rate_matrix
val stationary_distribution : param -> NSCodon.vector
