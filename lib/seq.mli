(** Module for sequences of bases. Provides a functor to build the module
    for any base module with the BASE signature.*)

module type Base = sig
  type t
  val to_char : t -> char
  val of_char_exn : char -> t
end

(** Module type for sequences of bases (eg, DNA). *)
module type S = sig
  type base
  type t
  val get : t -> int -> base
  val length : t -> int
  val of_list : base list -> t
  val of_string_exn : string -> t
  val to_string : t -> string
  val pp : Format.formatter -> t -> unit
end

(** Main functor (using arrays) *)
module Make (B : Base): S with type base = B.t

(** List-based constructor (no particular reason to use it) *)
module Make_list (B : Base): S with type base = B.t

(** Pre-built DNA module. *)
module DNA: module type of Make(Nucleotide)
