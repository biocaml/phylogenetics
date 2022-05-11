open Base

include Alphabet.Make(struct let card = 20 end)

let chars_of_aa_string = "ACDEFGHIKLMNPQRSTVWY"

let%test "chars_of_aa_string" =
  String.length chars_of_aa_string = card

let to_char i = chars_of_aa_string.[i]

let code_A = Char.to_int 'A'

let aa_of_chars =
  let t = Array.create ~len:26 None in
  for i = 0 to String.length chars_of_aa_string - 1 do
    t.(Char.to_int chars_of_aa_string.[i] - code_A) <- Some i
  done ;
  t

let of_char = function
  | 'A'..'Z' as c -> aa_of_chars.(Char.to_int c - code_A)
  | _ -> None

let of_char_exn c = Option.value_exn (of_char c)

let%test "of_char" =
  let open Poly in
  of_char (to_char 19) = Some 19
  && of_char 'Z' = None
