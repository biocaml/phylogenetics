(** A representation for sequence alignments

    An alignment is a non-empty collection of sequences where all
    sequences have the same length. Each sequence comes with a {i
    description}. A sequence description provides additional textual
    information about a sequence in an alignment, such as its name or
    biological context. It helps identify and differentiate sequences
    without directly inspecting the sequence data. *)

type t

val nrows : t -> int
(** [nrows alignment] returns the number of sequences in the
    alignment. *)

val ncols : t -> int
(** [ncols alignment] returns the number of columns in the alignment,
    i.e., the length of the sequences. *)

val description : t -> int -> string
(** [description alignment index] returns the description of the
    sequence at the given index in the alignment. *)

val sequence : t -> int -> string
(** [sequence alignment index] returns the sequence at the given index
    in the alignment. *)

type error = [
  | `Empty_alignment
  | `Unequal_sequence_lengths
]
[@@deriving show]
(** The possible errors that can occur when creating an alignment. *)

type parsing_error = [
  | `Syntax_error of string
  | error
]
[@@deriving show]

val map :
  t ->
  f:(description:string -> sequence:string -> string * string) ->
  (t, [> `Unequal_sequence_lengths]) result
(** [map alignment ~f] applies the function [f] to each sequence in
    the alignment and returns a new alignment with the modified
    descriptions and sequences. If the sequences have unequal lengths,
    an error of type [`Unequal_sequence_lengths] is returned. *)

val array_mapi :
  t ->
  f:(int -> description:string -> sequence:string -> 'a) ->
  'a array
(** [array_mapi alignment ~f] applies the function [f] to each
    sequence in the alignment, preserving the sequence indices, and
    returns an array of the resulting values. *)

val fold :
  t ->
  init:'a ->
  f:('a -> description:string -> sequence:string -> 'a) ->
  'a
(** [fold alignment ~init ~f] applies the function [f] to each
    sequence in the alignment using an initial accumulator [init], and
    returns the final value of the accumulator. *)

val find_sequence :
  t ->
  string ->
  string option
(** [find_sequence alignment id] searches for a sequence in the
    alignment using its description as the search criterion. It
    returns the corresponding sequence as a string option, or [None]
    if the sequence is not found. *)

val of_assoc_list : (string * string) list -> (t, [> error]) result
(** [of_assoc_list l] creates an alignment from a list of key-value
    associations, where the key represents the description of the
    sequence and the value represents the sequence itself. It returns
    the created alignment, or an error if the input data does not
    satisfy the alignment invariants. *)

module Fasta : sig
  val from_file :
    string ->
    (t, [> parsing_error]) result
  (** [from_file filename] reads an alignment from a FASTA file with the
      given [filename]. It returns the alignment as a result, or a
      parsing error if any error occurs during parsing. *)

  val from_file_exn : string -> t
  (** Same as {! from_file} but raises [Failure] if some error happens *)

  val to_channel : t -> out_channel -> unit
  (** [to_channel alignment oc] writes the alignment to the specified
      output channel [oc] in FASTA format. *)

  val to_file : t -> string -> unit
  (** [to_file alignment filename] writes the alignment to a FASTA file with the given [filename]. *)
end

module Phylip : sig
  val of_phylip : Phylip.t -> ( t, error) result

  val to_phylip : t -> Phylip.t

  val from_file : ?strict:bool -> string -> (t, parsing_error) result

  val from_file_exn : ?strict:bool -> string -> t
end

val indel_free_columns : t -> bool array
(** [indel_free_columns alignment] checks each column of the alignment
    for the presence of indels ("-"). It returns a boolean array
    indicating whether each column is indel-free. *)

val residues : t -> column:int -> Core.Char.Set.t
(** [residues alignment ~column:j] returns the set of residues present
    in the specified column [j] of the alignment. *)

val number_of_residues_per_column_stats : t -> (int * int) list
(** [number_of_residues_per_column_stats alignment] calculates the
    number of unique residues per column in the alignment and returns
    the statistics as a list of pairs, where the first element of each
    pair represents the column index and the second element represents
    the number of unique residues. *)

val composition : t -> (Char.t * float) list
(** [composition alignment] calculates the residue composition of the
    alignment and returns the results as a list of pairs, where each
    pair consists of a residue and its relative frequency in the
    alignment. *)

val constant_site : t -> int -> bool
(** [constant_site alignment j] checks whether the specified column
    [j] of the alignment contains a constant site, i.e., all sequences
    have the same residue at that column. *)

(** Legacy code: provides a functor to build from SEQUENCE modules. *)
open Sigs

module Make (S : Seq.S):
  ALIGNMENT with type base = S.base and type sequence = S.t
