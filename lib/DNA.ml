(** A simple module for DNA bases (not sequences)*)

open Printf

type dna = A | T | G | C

let string_of_dna = function A -> "A" | T -> "T" | G -> "G" | C -> "C"

(** Creates a DNA base from a string containing only a single
    letter (uppercase/lowercase).
    Raises invalid_arg in case of incorrect string parameter.*)
let dna_of_string = function
  | "A" | "a" -> A
  | "C" | "c" -> C
  | "G" | "g" -> G
  | "T" | "t" -> T
  | _ -> invalid_arg "dna_of_string"

let int_of_dna = function A -> 0 | C -> 1 | G -> 2 | T -> 3

(** Creates a DNA base from an int between 0 and 3
    Raises invalid_arg in case of incorrect int parameter.*)
let dna_of_int = function
  | 0 -> A
  | 1 -> C
  | 2 -> G
  | 3 -> T
  | x ->
    invalid_arg (sprintf "dna_of_int: %d is not a correct dna index" x)

let print_dna dna = print_string (string_of_dna dna)
