module type S = sig
  include Alphabet.S_int

  val to_string : t -> string
  val of_string : string -> t option
  val neighbours : t -> t -> (int * Nucleotide.t * Nucleotide.t) option
  val nucleotides : t -> Nucleotide.t * Nucleotide.t * Nucleotide.t
end

module type Genetic_code = sig
  type codon
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

include S
module Universal_genetic_code : Genetic_code with type codon := t
