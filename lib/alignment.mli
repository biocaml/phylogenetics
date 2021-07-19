open Core_kernel

type t = private {
  descriptions : string array ;
  sequences : string array ;
}
type parsing_error = [
  | `Fasta_parser_error of int * string
  | `Msg of string
]
[@@deriving show]

val from_fasta :
  string ->
  (t, [> parsing_error]) result
val indel_free_columns : t -> bool array
val nrows : t -> int
val ncols : t -> int

val find_sequence :
  t ->
  string ->
  string option

val residues : t -> column:int -> Char.Set.t
val number_of_residues_per_column_stats : t -> (int * int) list
val composition : t -> (Char.t * float) list
val constant_site : t -> int -> bool
(** Module for alignments (sets of aligned sequences with same length).
    Provides a functor to build from SEQUENCE modules. *)

open Sigs

(** Main functor ; uses a hashtable. *)
module Make (S : Seq.S):
  ALIGNMENT with type base = S.base and type sequence = S.t
