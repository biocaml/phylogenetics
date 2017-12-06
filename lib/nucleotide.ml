type t = A | C | G | T

(** Returns a string with a single capital letter representing the base. *)
let to_string = function A -> "A" | T -> "T" | G -> "G" | C -> "C"

(** Creates a DNA base from a char (case insensitive).
    Raises invalid_arg in case of incorrect char parameter.*)
let of_char = function
  | 'A' | 'a' -> A
  | 'C' | 'c' -> C
  | 'G' | 'g' -> G
  | 'T' | 't' -> T
  | _ -> invalid_arg "base_of_char"

(** Converts a base into an int (alphabetical order, starting at 0) *)
let to_int = function A -> 0 | G -> 1 | C -> 2 | T -> 3

(** Creates a DNA base from an int between 0 and 3
    Raises invalid_arg in case of incorrect int parameter.*)
let of_int = function
  | 0 -> A
  | 1 -> G
  | 2 -> C
  | 3 -> T
  | x ->
    invalid_arg (Printf.sprintf "base_of_int: %d is not a correct base index" x)

(** Prints single base without line break. *)
let print_base base = print_string (to_string base)

let alphabet_size = 4
