(** WAG matrix parser

    Parser for the WAG matrix as available {{:https://www.ebi.ac.uk/goldman-srv/WAG/}here}.
*)

type t = {
  rate_matrix : Amino_acid.matrix ;
  freqs : Amino_acid.vector ;
}

val parse : string -> t
