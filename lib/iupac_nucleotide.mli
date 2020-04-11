(** Nucleotide IUPAC symbol *)

include Alphabet.S_int

val of_char : char -> t option
val is_ambiguous : t -> bool
