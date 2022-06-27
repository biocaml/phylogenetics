module type S = sig
  include Alphabet.S_int

  val to_string : t -> string
  val of_string : string -> t option
  val neighbours : t -> t -> (int * Nucleotide.t * Nucleotide.t) option
  val nucleotides : t -> Nucleotide.t * Nucleotide.t * Nucleotide.t
end

type genetic_code
(** Genetic codes specified by {{:https://www.ncbi.nlm.nih.gov/Taxonomy/Utils/wprintgc.cgi?mode=c}this NCBI page} *)

val genetic_codes : genetic_code list
val transl_table : genetic_code -> int
val label_of_genetic_code : genetic_code -> string

include S

module type Genetic_code = sig
  type codon = t
  val stop_codons : codon list
  val is_stop_codon : codon -> bool
  val aa_of_codon : codon -> Amino_acid.t option
  val synonym : codon -> codon -> bool

  (** subset of non-stop codons *)
  module NS : sig
    include S
    val to_codon : t -> codon
    val aa_of_codon : t -> Amino_acid.t
    val synonym : t -> t -> bool
    val of_int_exn : int -> t
  end
end

module Universal_genetic_code : Genetic_code

val genetic_code_impl : genetic_code -> (module Genetic_code)
