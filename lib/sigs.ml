(** Compilation of module signatures used elsewhere. *)

open Linear_algebra (* to get the vec and mat types *)

(** Index type for tree leaves and sequences in alignments *)
type index = string

(** Module type for individual bases (eg, A, T, C, G).
    Mostly conversion to/from strings and ints *)
module type BASE = sig
  type t
  val of_char: char -> t
  val of_int: int -> t
  val to_int: t -> int
  val print_base: t -> unit
  val to_string: t -> string
  val alphabet_size: int
end

(** Module type for sequences of bases (eg, DNA). *)
module type SEQUENCE = sig
  type base
  type t
  val get: t -> int -> base
  val length: t -> int
  val of_list: base list -> t
  val of_string: string -> t
  val to_string: t -> string
  val pp: Format.formatter -> t -> unit
end

(** Module type for alignments of sequences (eg, DNA alignment.) *)
module type ALIGNMENT = sig
  type t
  type base
  type sequence
  val of_string_list: string list -> t
  val of_assoc_list: (index*sequence) list -> t
  val of_fasta: string -> t
  val pp: Format.formatter -> t -> unit
  val get_base: t -> seq:index -> pos:int -> base
  val length: t -> int
  val nb_seq: t -> int
  val to_file: t -> string -> unit
  val equal: t -> t -> bool
end


(** Evolution model with linear algebra functions to compute static distribution and
    transition matrix diagonalization.*)
module type EVOL_MODEL = sig
  type t
  type base
  val transition: t -> base -> base -> float
  val of_string: string -> t
  val to_string: t -> string
  val eMt_mat: t -> float -> mat
  val eMt_series: t -> float -> mat
  val stat_dist_vec: t -> vec
  val known_vector: base -> vec
end
