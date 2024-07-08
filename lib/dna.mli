(** Strings that represent valid DNA sequences *)

type t = private string

val of_string_unsafe : string -> t
(** [of_string_unsafe s] creates a DNA sequence from a string without validation.
    {[
      let dna_sequence = of_string_unsafe "ACGT" in
      (* dna_sequence is now "ACGT". If an unexpected character is encountered, the result is undefined. *)
    ]}
*)

val of_string_exn : string -> t
(** [of_string_exn s] creates a DNA sequence from a string, raising an
    exception if an unexpected character is encountered. Raises
    [Invalid_argument] if an unexpected character is found.

    Example:
    {[
      let dna_sequence = of_string_exn "ACGT" in
      (* dna_sequence is now "ACGT" *)
    ]}
*)

val of_codons : Codon.Universal_genetic_code.NS.t array -> t
(** [of_codons codons] creates a DNA sequence from an array of codons.

    Example:

    {[
      let codons = [|Codon.Universal_genetic_code.NS.Adenine; Codon.Universal_genetic_code.NS.Cytosine; Codon.Universal_genetic_code.NS.Guanine; Codon.Universal_genetic_code.NS.Thymine|] in
      let dna_sequence = of_codons codons in
      (* dna_sequence is now "ACGT" *)
    ]}
*)

val gc_contents : t -> float
(** [gc_contents s] calculates the GC (Guanine/Cytosine) contents of a
    DNA sequence, that is it counts the occurrences of 'C' and 'G' in
    [s] and returns the ratio of GC bases to the total length of the
    sequence.

    Example:

    {[
      let dna_sequence = of_string_exn "ACGT" in
      let gc_content = gc_contents dna_sequence in
      (* gc_content is approximately 0.5 *)
    ]}
*)
