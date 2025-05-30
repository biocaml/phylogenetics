open Core

module AA = Amino_acid

let chars_of_aa_string = "ACDEFGHIKLMNPQRSTVWYBJZ"


include Alphabet.Make(struct let card = 23 end)

let to_char i = chars_of_aa_string.[i]

let code_A = Char.to_int 'A'
(** ASCII code for 'A' *)

let aa_of_chars =
  let t = Array.create ~len:26 None in
  for i = 0 to card - 1 do
    t.(Char.to_int chars_of_aa_string.[i] - code_A) <- Some i
  done ;
  t

let of_char = function
  | 'A'..'Z' as c -> aa_of_chars.(Char.to_int c - code_A)
  | _ -> None

let of_char_exn c = Option.value_exn (of_char c)

let aa_B = of_char_exn 'B'
let aa_J = of_char_exn 'J'
let aa_Z = of_char_exn 'Z'
let aa_D = AA.of_char_exn 'D'
let aa_N = AA.of_char_exn 'N'
let aa_I = AA.of_char_exn 'I'
let aa_L = AA.of_char_exn 'L'
let aa_E = AA.of_char_exn 'E'
let aa_Q = AA.of_char_exn 'Q'

let of_amino_acid (aa : Amino_acid.t) : t = (aa :> int)

let to_amino_acid x =
  Option.some_if (x < AA.card) (Amino_acid.of_int_exn (x:>int))

let fold ~init x ~f =
  if x < Amino_acid.card then f init (Amino_acid.of_int_exn x)
  else if x = aa_B then f (f init aa_D) aa_N
  else if x = aa_J then f (f init aa_I) aa_L
  else if x = aa_Z then f (f init aa_E) aa_Q
  else assert false

let multiplicity x = if x < AA.card then 1 else 2

let mem (x:t) (aa:AA.t) =
  let aa = (aa :> int) in
  if x < AA.card then equal (x :> int) aa
  else if x = aa_B then (aa = (aa_N :> int) || aa = (aa_D :> int))
  else if x = aa_J then (aa = (aa_I :> int) || aa = (aa_L :> int))
  else if x = aa_Z then (aa = (aa_E :> int) || aa = (aa_Q :> int))
  else assert false

let%test "card" = (card = String.length chars_of_aa_string)

let%test "AA card" = (card = AA.card + 3)

let%test "mem" =
  mem aa_B (AA.of_char_exn 'D') &&
  mem aa_B (AA.of_char_exn 'N') &&
  mem aa_J (AA.of_char_exn 'I') &&
  mem aa_J (AA.of_char_exn 'L') &&
  mem aa_Z (AA.of_char_exn 'Q') &&
  mem aa_Z (AA.of_char_exn 'E')

let%test "of_char" =
  let open Poly in
  of_char (to_char 19) = Some 19
  && of_char 'Z' = Some aa_Z
  && of_char 'U' = None

let%test "to_char" =
  Char.(to_char aa_Z = 'Z') &&
  Char.(to_char aa_B = 'B') &&
  Char.(to_char aa_J = 'J') &&
  Char.(to_char (of_amino_acid aa_D) = 'D')

let%test "AA equivalence" =
  (of_char_exn 'D') = (aa_D :> int) &&
  (of_char_exn 'D') = ((AA.of_char_exn 'D') :> int)
