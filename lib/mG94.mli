(** Codon rate matrix from Muse and Gaut 94

    This modules provides functions to build rates matrices for codon
    evolution following the model proposed by Muse and Gaut [1]. The
    exact formulation follows the parameterization used by Latrille
    and Lartillot [2].

    References:
    @see <https://academic.oup.com/mbe/article/11/5/715/1008710>
    @see <https://academic.oup.com/mbe/article/34/1/204/2656188>

*)

module NSCodon = Codon.Universal_genetic_code.NS
module NSCodon_rate_matrix : module type of Rate_matrix.Make(NSCodon)

type param = {
  nucleotide_rates : Rate_matrix.Nucleotide.t ;
  nucleotide_stat_dist : Nucleotide.vector ;
  omega : float ;
}

val rate_matrix : param -> NSCodon_rate_matrix.t
val stationary_distribution : param -> NSCodon.vector
