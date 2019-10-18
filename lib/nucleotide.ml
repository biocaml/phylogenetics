include Alphabet.Make(struct let card = 4 end)

let a = 0
let c = 1
let g = 2
let t = 3

let of_char_exn = function
  | 'a' | 'A' -> 0
  | 'c' | 'C' -> 1
  | 'g' | 'G' -> 2
  | 't' | 'T' -> 3
  | _ -> invalid_arg "Nucleotide.of_char_exn"

let transversion p q =
  match p, q with
  | 0, 2 | 2, 0
  | 1, 3 | 3, 1 -> false
  | _ -> true

type repr = A | C | G | T

let inspect = function
  | 0 -> A
  | 1 -> C
  | 2 -> G
  | 3 -> T
  | _ -> assert false

let to_char = function
  | 0 -> 'A'
  | 1 -> 'C'
  | 2 -> 'G'
  | 3 -> 'T'
  | _ -> assert false
