(** Abstract representations of codons and genetic codes

    Codons are triplets of nucleotides that encode amino acids in a
    coding sequence. The correspondance between codons and amino acids
    is called a {i genetic code} and several have been observed in living beings.

    This module includes an implementation for the universal genetic
    code as well as functions to work with genetic codes specified by
    the NCBI
    ({{:https://www.ncbi.nlm.nih.gov/Taxonomy/Utils/wprintgc.cgi?mode=c}this
    NCBI page}).

    The module also defines types and functions to manipulate codons and nucleotides.
*)


module type S = sig
  include Alphabet.S_int


  val to_string : t -> string
  (** [to_string c] returns a string representation of codon [c]

      Example:

      {[
        let codon_option = Codon.of_string "ATG" in
        match codon_option with
        | Some codon -> assert (Codon.to_string codon = "ATG")
        | None -> failwith "Invalid codon string"
      ]}

      In this example, the codon is converted to its string
      representation. The string representation is then compared to
      the expected value.  *)

  val of_string : string -> t option
  (** [of_string s] tries tp build a codon from a string
      representation. It returns [None] if the string is not a valid
      codon *)

  val neighbours : t -> t -> (int * Nucleotide.t * Nucleotide.t) option
  (** [neighbours p q] tests if codons [p] and [q] are neighbors that
      is, if they differ by exactly one nucleotide. If so, the
      function returns the index of the differing nucleotide and the
      nucleotides themselves; it returns [None] if the codons are not
      neighbors.

      Example:

      {[
        let codon_p = Codon.of_string "ATA" in
        let codon_q = Codon.of_string "ATG" in
        match Codon.neighbours codon_p codon_q with
        | Some (index, nucleotide_p, nucleotide_q) ->
          assert (index = 2);
          assert (nucleotide_p = Nucleotide.A);
          assert (nucleotide_q = Nucleotide.G)
        | None -> failwith "Codons are not neighbors"
      ]}

      In this example, the codons are compared to find the index of
      the differing nucleotide and the nucleotides themselves. The
      index and nucleotides are then compared to the expected values.
  *)

  val nucleotides : t -> Nucleotide.t * Nucleotide.t * Nucleotide.t
  (** [nucleotides c] returns the triplet of nucleotides of [c]

      Example :

      {[
        let codon_option = Codon.of_string "ATG" in
        match codon_option with
        | None -> failwith "Invalid codon string"
        | Some codon ->
          let (n1, n2, n3) = Codon.nucleotides codon in
          assert (n1 = Nucleotide.A);
          assert (n2 = Nucleotide.T);
          assert (n3 = Nucleotide.G)
      ]}

      In this example, the nucleotides of the codon are extracted and
      compared to the expected values.  *)
end

type genetic_code
(** A type representing a genetic code

    This type represents a genetic code as specified
    {{:https://www.ncbi.nlm.nih.gov/Taxonomy/Utils/wprintgc.cgi?mode=c}this
    NCBI page}.

    Each genetic code is identified by a unique integer identifier and
    has a label describing its origin.  It is associated with a string
    representing the amino acids encoded by the codons.  *)

val genetic_codes : genetic_code list
(** List of genetic codes specified by the NCBI.

    The list contains tuples with the following components:
    - Genetic code identifier (integer)
    - Genetic code label (string)
    - String representation of the amino acids encoded by the codons
*)

val transl_table : genetic_code -> int
(** Translation table of a genetic code to its identifier*)

val label_of_genetic_code : genetic_code -> string

include S

module type Genetic_code = sig
  type codon = t
  val stop_codons : codon list

  val is_stop_codon : codon -> bool
  (** [is_stop_codon c] is true if [c] is a stop codon.

      Example :

      {[
        let codon_option = Codon.of_string "TAA" in
        match codon_option with
        | None -> failwith "Invalid codon string"
        | Some codon ->
          assert (Codon.is_stop_codon codon = true)
      ]}


      A stop codon is a specific codon in the genetic code that
      signals the termination of protein synthesis. When a stop codon
      is encountered during translation, the ribosome releases the
      newly synthesized polypeptide chain and the translation process
      comes to a halt. In the universal genetic code, there are three
      stop codons: "TAA", "TAG", and "TGA". These codons do not encode
      any amino acid and serve as termination signals for the protein
      synthesis machinery. *)

  val aa_of_codon : codon -> Amino_acid.t option
  (** [aa_of_codon c] is -- it exists -- the amino acid encoded by [c]

      {[
        let codon = Codon.of_string "ATG" in
        let amino_acid_option = Codon.aa_of_codon codon in
        match amino_acid_option with
        | Some amino_acid -> assert (amino_acid = Amino_acid.Met)
        | None -> failwith "No matching amino acid found"
      ]}
  *)

  val synonym : codon -> codon -> bool
  (** Tests if two codons are synonymous.

      Example :

      {[
        let codon1_option = Codon.of_string "ATG" in
        let codon2_option = Codon.of_string "ATA" in
        match codon1_option, codon2_option with
        | None, _ | _, None -> failwith "Invalid codon string"
        | Some codon1, Some codon2 ->
          assert (Codon.synonym codon1 codon2 = false)
      ]}

      In this example, the codons are compared to check if they are synonymous.
      Two codons are synonymous if they encode the same amino acid.
  *)

  (** {3 Non-stop Codons module}

      The module type for the subset of non-stop codons.

      This module provides functions specific to the non-stop codons subset of a genetic code,
      including retrieving the amino acid encoded by a codon (without the need for option type)
      and converting an integer to a codon.
  *)
  module NS : sig
    include S
    val to_codon : t -> codon

    val aa_of_codon : t -> Amino_acid.t
    (** Get the amino acid encoded by a codon.

        Example :

        {[
          let codon = Codon.NS.to_codon (Nucleotide.A, Nucleotide.T, Nucleotide.G) in
          let amino_acid = Codon.NS.aa_of_codon codon in
          assert (amino_acid = Amino_acid.Met)
        ]}
    *)

    val synonym : t -> t -> bool

    val of_int_exn : int -> t
    (** Convert an integer to a codon.

        [n] The integer
        Returns: The codon
        Raises: Invalid_argument if the integer is invalid
    *)
  end
end

module Universal_genetic_code : Genetic_code
(** Universal genetic code module.

    This module provides an implementation of the universal genetic
    code, which is the default genetic code used in most organisms.

    The universal genetic code consists of 64 codons, out of which 61
    encode specific amino acids, and the remaining 3 codons serve as
    stop signals, indicating the termination of protein synthesis.
    The specific mapping between codons and amino acids is the same
    across different species, allowing genetic information to be
    accurately transferred and translated into proteins.

    This module provides an implementation of the universal genetic
    code, including functions to check for stop codons, retrieve the
    amino acid encoded by a codon, and determine if two codons are
    synonymous.
*)

val genetic_code_impl : genetic_code -> (module Genetic_code)
(** Get the implementation module for a genetic code.

    {[
      let module Genetic_code = Codon.genetic_code_impl genetic_code in
      let stop_codons = Genetic_code.stop_codons in
      assert (List.length stop_codons = 3)
    ]}
*)
