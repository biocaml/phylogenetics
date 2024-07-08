(** Integer-based representation for amino acids

    This module implements an amino acid alphabet, which is a specific
    instance of the `Alphabet.S_int` module. It represents the set of
    20 standard amino acids.

    The alphabet is represented as an integer alphabet, where each
    amino acid is assigned a unique integer index.

    Conversion functions are provided to convert between the integer
    index and the corresponding character representation of an amino
    acid.

    Note: The integer indices of amino acids start from 0 and go up to 19.
*)

include Alphabet.S_int

val yojson_of_vector : vector -> Yojson.Safe.t
(** [yojson_of_vector vec] converts a vector representing the values
    of amino acids into a Yojson.Safe.t JSON object.

    {[
      let amino_acid_values = [|1.2; 3.4; 2.1|]  (* Vector representing amino acid values *)
      let json_repr = yojson_of_vector amino_acid_values  (* Conversion to JSON object *)
    ]}

    In this example, if [amino_acid_values] represents the values of three amino acids in the order
    "Alanine", "Aspartic Acid", and "Cysteine", then [json_repr] will be a JSON object with the following structure:

    {[
      {
        "A": 1.2,
        "D": 3.4,
        "C": 2.1
      }
    ]}

    Each amino acid is represented as a key-value pair in the JSON object, where the key is the character representation
    and the value is the corresponding value in the vector.

    This function is useful when you need to serialize amino acid values in a JSON format for storage or communication purposes.
*)

val vector_of_yojson : Yojson.Safe.t -> vector
(** [vector_of_yojson aa_list] converts a JSON object of type
    `Yojson.Safe.t` into a vector of amino acid values. The JSON
    object should contain key-value pairs, where the key is the
    character representation of an amino acid and the value is the
    corresponding value. The order of the key-value pairs should
    follow the alphabetical order. If any amino acid is missing or the
    order is not correct, an exception is raised. *)

val to_char : t -> char
(** [to_char aa] converts the integer index of an amino acid [aa] to
    its character representation. *)

val of_char : char -> t option
(** [of_char c] converts a character representation [c] of an amino
    acid to its corresponding integer index, returning an option. If
    the character is not a valid amino acid, [None] is returned. *)

val of_char_exn : char -> t
(** Same as {! of_char} but raises an exception if some error
    happens *)
