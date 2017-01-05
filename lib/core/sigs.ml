(** Module to handle individual bases (eg, A, T, C, G).
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

module type ALIGNMENT = sig
  type t
  type base
  type sequence
  val of_string_list: string list -> t
  val of_assoc_list: (int*sequence) list -> t
  val of_fasta: string -> t
  val pp: Format.formatter -> t -> unit
  val get_base: t -> seq:int -> pos:int -> base
  val length: t -> int
  val to_file: t -> string -> unit
  val equal: t -> t -> bool
end

(* evolution models  *)
module type TRANSITION_MATRIX = sig
  type t
  module Base:BASE
  val transition: t -> Base.t -> Base.t -> float
  val of_string: string -> t
  val to_string: t -> string
end

module type EVOL_MODEL = sig
  include TRANSITION_MATRIX
  val stat_dis: t -> Base.t -> float
  val has_decomposition: bool
  val diag: t -> int -> float
  val diag_p: t -> int -> int -> float
  val diag_p_inv: t -> int -> int -> float
end
