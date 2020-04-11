open Core_kernel

include Alphabet.Make(struct let card = 16 end)

let chars_of_aa_string = "ACGTRYSWKMBDHVN"

let code_A = Char.to_int 'A'

let iupac_of_chars =
  let t = Array.create ~len:26 None in
  for i = 0 to String.length chars_of_aa_string - 1 do
    t.(Char.to_int chars_of_aa_string.[i] - code_A) <- Some i
  done ;
  t

let of_char = function
  | 'A'..'Z' as c -> iupac_of_chars.(Char.to_int c - code_A)
  | _ -> None

let is_ambiguous c = c > 3

let%test "IUPAC ambiguity" =
  Option.map (of_char 'R') ~f:is_ambiguous
  |> Option.value ~default:false
