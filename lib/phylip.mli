(** Parsing for the PHYLIP format

    This module provides functionality for working with the [Phylip]
    file format, which is commonly used in phylogenetic analysis and
    sequence alignment. The [Phylip] format is a plain text format
    that represents multiple biological sequences along with their
    associated metadata. The original format is described
    {{:http://evolution.genetics.washington.edu/phylip.html}there}. This
    implementation also allows a somewhat more relaxed syntax:

{v
<nb sequences> <nb cols>
<id> TAB <sequence>
<id> TAB <sequence>
...
v}
    which is specified with the option [~strict:false].

    Example usage of the functions:
    {[
      let items = [
        { name = "Seq1"; sequence = "ACGT" };
        { name = "Seq2"; sequence = "CGTA" };
      ] in
      let data = make_exn items in
      write data "output.txt";
      let parsed_data = read "output.txt" in
      match parsed_data with
      | Ok parsed -> Printf.printf "Number of sequences: %d\n" parsed.number_of_sequences
      | Error (`Msg msg) -> Printf.printf "Parsing error: %s\n" msg
    ]}
*)

type item = {
  name : string ;
  sequence : string ;
}

type t = private {
  number_of_sequences : int ;
  sequence_length : int ;
  items : item list ;
}

val make_exn : item list -> t
(** [make_exn items] creates a new [t] value from a list of items.
   It validates the input by checking that the list is not empty and
   that all sequences have the same length.
   Raises [Invalid_argument] if the list of items is empty or if
   the sequences have different lengths. *)

val read :
  ?strict:bool ->
  string ->
  (t, [> `Msg of string]) result
(** [read ?strict fn] reads the contents of the specified file [fn]
    and returns a [t] value representing the data. The [strict]
    parameter specifies whether to use strict parsing or relaxed
    parsing. If [strict] is set to [true], the function expects the
    file to follow the strict format. If [strict] is set to [false],
    the function allows a more relaxed syntax.

    Returns:
    - [Ok t] if the file is successfully parsed and the [t] value is
    created.
    - [Error (`Msg msg)] if an error occurs during parsing, with [msg]
    describing the error. *)

val read_exn :
  ?strict:bool ->
  string ->
  t
(** [read_exn ?strict fn] is similar to [read], but raises an
    exception with the error message if the parsing fails. *)

val write :
  ?strict:bool ->
  t ->
  string ->
  unit
(** [write ?strict t fn] writes the data from the [t] value to the
    specified file [fn]. The [strict] parameter specifies whether to
    use strict writing or relaxed writing. If [strict] is set to
    [true], the function writes the data in strict format. If [strict]
    is set to [false], the function writes the data in a more relaxed
    format. *)
