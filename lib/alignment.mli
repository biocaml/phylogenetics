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

val fold :
  t ->
  init:'a ->
  f:('a -> description:string -> sequence:string -> 'a) ->
  'a

val find_sequence :
  t ->
  string ->
  string option

type parsing_error = [
  | `Fasta_parser_error of string
  | error
]
[@@deriving show]

val of_assoc_list : (string * string) list -> (t, [> error]) result

val from_fasta :
  string ->
  (t, [> parsing_error]) result

val to_fasta : t -> string -> unit

val indel_free_columns : t -> bool array

val residues : t -> column:int -> Core_kernel.Char.Set.t
val number_of_residues_per_column_stats : t -> (int * int) list
val composition : t -> (Char.t * float) list
val constant_site : t -> int -> bool

open Sigs

(** Legacy code: provides a functor to build from SEQUENCE modules. *)
module Make (S : Seq.S):
  ALIGNMENT with type base = S.base and type sequence = S.t
