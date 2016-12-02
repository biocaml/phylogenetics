module type BASE = sig
  type base
  val base_of_string: string -> base
  val base_of_int: int -> base
  val int_of_base: base -> int
  val print_base: base -> unit
  val string_of_base: base -> string
end

type dna = A | T | G | C

module DNA:(BASE with type base = dna) = struct
  open Printf

  type base = dna

  let string_of_base = function A -> "A" | T -> "T" | G -> "G" | C -> "C"

  (** Creates a DNA base from a string containing only a single
      letter (uppercase/lowercase).
      Raises invalid_arg in case of incorrect string parameter.*)
  let base_of_string = function
    | "A" | "a" -> A
    | "C" | "c" -> C
    | "G" | "g" -> G
    | "T" | "t" -> T
    | _ -> invalid_arg "base_of_string"

  let int_of_base = function A -> 0 | C -> 1 | G -> 2 | T -> 3

  (** Creates a DNA base from an int between 0 and 3
      Raises invalid_arg in case of incorrect int parameter.*)
  let base_of_int = function
    | 0 -> A
    | 1 -> C
    | 2 -> G
    | 3 -> T
    | x ->
      invalid_arg (sprintf "base_of_int: %d is not a correct base index" x)

  let print_base base = print_string (string_of_base base)
end

module type SEQUENCE = sig
  include BASE
  type sequence = base list
  type sequence_table = (int * sequence) list
  val get_base: int -> int -> sequence_table -> base
  val seq_of_string: string -> sequence
  val table_of_string_list: string list -> sequence_table
end

module Sequence (Base:BASE):(SEQUENCE with type base=Base.base)  = struct
  include Base
  type sequence = base list
  type sequence_table = (int * sequence) list

  let get_base i site seq = List.nth (List.assoc i seq) site

  let seq_of_string str =
    let rec aux i acc =
      if (i >= String.length str) then
        List.rev acc
      else
        match String.sub str i 1 |> base_of_string with
        | b -> aux (i+1) (b::acc)
        | exception e -> invalid_arg "input string"
    in
    aux 0 []

  let table_of_string_list l =
    let rec aux acc i = function
      | [] -> List.rev acc
      | s::t -> aux ((i, (seq_of_string s))::acc) (i+1) t
    in aux [] 0 l
end

module DNA_Sequence = Sequence (DNA)
