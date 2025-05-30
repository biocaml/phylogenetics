(** An extended amino acid alphabet, including B,J and Z equivoque symbols *)


include Alphabet.S_int

val to_char : t -> char
val of_char : char -> t option
val of_char_exn : char -> t
val of_amino_acid : Amino_acid.t -> t

(** [to_amino_acid x] is Some amino acid iff [x] is univoque *)
val to_amino_acid : t -> Amino_acid.t option

(** [mem x aa] tests that [x] denotes [aa] *)
val mem : t -> Amino_acid.t -> bool
val fold : init:'a -> t -> f:('a -> Amino_acid.t -> 'a) -> 'a
val multiplicity : t -> int
