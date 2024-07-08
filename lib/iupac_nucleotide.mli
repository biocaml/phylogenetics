(** Nucleotide IUPAC symbol

    IUPAC symbols are characters that can be used to represent a
    subset of nucleotides, for instance when the nucleotide at a given
    position is not precisely known. A IUPAC symbol is said {i
    ambiguous} if it corresponds to a set of size at least 2.

    Valid IUPAC symbols for nucleotides can be found {{:
    https://www.bioinformatics.org/sms/iupac.html} here}.
*)

include Alphabet.S_int

val of_char : char -> t option
(** [of_char c] returns the IUPAC symbol corresponding to the given
    character [c].  It returns [None] if the character is not a valid
    IUPAC nucleotide symbol.

    Example:
    {[
      let symbol = Iupac_nucleotide.of_char 'A' in
      (* symbol = Some 0 *)
    ]}
*)

val is_ambiguous : t -> bool
(** [is_ambiguous sym] checks if the given IUPAC symbol [sym]
    represents an ambiguous nucleotide.  It returns [true] if the
    symbol is ambiguous, and [false] otherwise.

    Example:
    {[
      let ambiguous = Iupac_nucleotide.is_ambiguous 4 in
      (* ambiguous = true *)
    ]}
*)
