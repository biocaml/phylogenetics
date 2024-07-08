(** A single DNA base *)

include Alphabet.S_int

val a : t
(** The base 'A'. *)

val c : t
(** The base 'C'. *)

val g : t
(** The base 'G'. *)

val t : t
(** The base 'T'. *)

val of_char_exn : Char.t -> t
(** Creates a DNA base from a char (case insensitive).
    Raises invalid_arg in case of incorrect char parameter.*)

val to_char : t -> Char.t
(** Returns a single capital character representing the base. *)

val transversion : t -> t -> bool
(** [transversion x y] is [true] if [x] and [y] belong to different
    purine/pyrimidine groups. *)

type repr = A | C | G | T

val inspect : t -> repr
(** Returns the representation of a DNA base as a variant type.
    The representation can be 'A', 'C', 'G', or 'T'. *)
