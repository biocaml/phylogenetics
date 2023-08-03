(** A representation for sequence alignments

    The following invariants are maintained:
    - the alignment is non-empty
    - all sequences have the same length *)

type t

val nrows : t -> int
val ncols : t -> int

val description : t -> int -> string
val sequence : t -> int -> string

type error = [
  | `Empty_alignment
  | `Unequal_sequence_lengths
]
[@@deriving show]

val map :
  t ->
  f:(description:string -> sequence:string -> string * string) ->
  (t, [> `Unequal_sequence_lengths]) result

val array_mapi :
  t ->
  f:(int -> description:string -> sequence:string -> 'a) ->
  'a array

val fold :
  t ->
  init:'a ->
  f:('a -> description:string -> sequence:string -> 'a) ->
  'a

val find_sequence :
  t ->
  string ->
  string option



val of_assoc_list : (string * string) list -> (t, [> error]) result

module Fasta : sig
  type parsing_error = [
    | `Fasta_parser_error of string
    | error
  ]
  [@@deriving show]

  val from_file :
    string ->
    (t, [> parsing_error]) result

  val from_file_exn : string -> t
  (** Same as {! from_file} but raises [Failure] if some error happens *)

  val to_channel : t -> out_channel -> unit
  val to_file : t -> string -> unit
end

module Phylip : sig
  type parsing_error = [
    | `Phylip_parser_error of string
    | error
  ]
  [@@deriving show]

  val of_phylip : Phylip.t -> ( t, error) result

  val to_phylip : t -> Phylip.t

  val from_file : ?strict:bool -> string -> (t, parsing_error) result

  val from_file_exn : string -> t
end

val indel_free_columns : t -> bool array

val residues : t -> column:int -> Core.Char.Set.t
val number_of_residues_per_column_stats : t -> (int * int) list
val composition : t -> (Char.t * float) list
val constant_site : t -> int -> bool

open Sigs

(** Legacy code: provides a functor to build from SEQUENCE modules. *)
module Make (S : Seq.S):
  ALIGNMENT with type base = S.base and type sequence = S.t
