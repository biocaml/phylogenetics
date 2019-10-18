(** A single DNA base *)

include Alphabet.S_int

val a : t
val c : t
val g : t
val t : t

(** Creates a DNA base from a char (case insensitive).
    Raises invalid_arg in case of incorrect char parameter.*)
val of_char_exn : Char.t -> t

(** Returns a single capital character representing the base. *)
val to_char : t -> Char.t

val transversion : t -> t -> bool

type repr = A | C | G | T

val inspect : t -> repr
