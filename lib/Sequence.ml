(** This modules holds a collection of functors and modules
    to handle sequences, bases and sequence tables. *)

(** Module to handle individual bases (eg, A, T, C, G).
    Mostly conversion to/from strings and ints *)
module type BASE = sig
  type base
  val base_of_char: char -> base
  val base_of_int: int -> base
  val int_of_base: base -> int
  val print_base: base -> unit
  val string_of_base: base -> string
end

(** A single DNA base *)
type dna = A | T | G | C

(** DNA-specific implementation of the BASE signature *)
module DNA:(BASE with type base = dna) = struct
  open Printf

  type base = dna

  (** Returns a string with a single capital letter representing the base. *)
  let string_of_base = function A -> "A" | T -> "T" | G -> "G" | C -> "C"

  (** Creates a DNA base from a char (case insensitive).
      Raises invalid_arg in case of incorrect char parameter.*)
  let base_of_char = function
    | 'A' | 'a' -> A
    | 'C' | 'c' -> C
    | 'G' | 'g' -> G
    | 'T' | 't' -> T
    | _ -> invalid_arg "base_of_char"

  (** Converts a base into an int (alphabetical order, starting at 0) *)
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

  (** Prints single base without line break. *)
  let print_base base = print_string (string_of_base base)
end

module type SEQUENCE = sig
  include BASE
  type sequence = base list
  type sequence_table = (int * sequence) list
  val get_base: int -> int -> sequence_table -> base
  val seq_of_string: string -> sequence
  val table_of_string_list: string list -> sequence_table
  val string_of_seq: sequence -> string
  val pp_seq: Format.formatter -> sequence -> unit
  val pp_table: Format.formatter -> sequence_table -> unit
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
        match base_of_char str.[i] with
        | b -> aux (i+1) (b::acc)
        | exception e -> invalid_arg "input string"
    in
    aux 0 []

  let table_of_string_list l =
    let rec aux acc i = function
      | [] -> List.rev acc
      | s::t -> aux ((i, (seq_of_string s))::acc) (i+1) t
    in aux [] 0 l

  let string_of_seq seq = List.map string_of_base seq |> String.concat ""

  let pp_seq fmt seq = string_of_seq seq |> Format.fprintf fmt "%s"

  let pp_table fmt tab =
    List.map (function (x,y) -> Printf.sprintf "%d:%s" x (string_of_seq y)) tab
    |> String.concat ";"
    |> Format.fprintf fmt "%s"
end

module DNA_Sequence = Sequence (DNA)
