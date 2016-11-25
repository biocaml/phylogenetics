open Printf

type dna = A | T | G | C

let string_of_dna = function A -> "A" | T -> "T" | G -> "G" | C -> "C"

let dna_of_string = function
  | "A" -> A
  | "C" -> C
  | "G" -> G
  | "T" -> T
  | _ -> invalid_arg "dna_of_string"

let int_of_dna = function A -> 0 | C -> 1 | G -> 2 | T -> 3

let dna_of_int = function
  | 0 -> A
  | 1 -> C
  | 2 -> G
  | 3 -> T
  | x ->
    invalid_arg (sprintf "dna_of_int: %d is not a correct dna index" x)

let print_dna dna = print_string (string_of_dna dna)
