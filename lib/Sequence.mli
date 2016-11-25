type base = DNA.dna

type sequence = (int * base) list

val get_base: int -> sequence -> base
