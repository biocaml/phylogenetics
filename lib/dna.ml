open Core_kernel

type t = string

let of_string_unsafe x = x

let of_string_exn s =
  match String.find s ~f:(function
      | 'a' | 'A' | 'c' | 'C'
      | 'g' | 'G' | 't' | 'T' -> false
      | _ -> true
    )
  with
  | None -> s
  | Some c -> invalid_argf "of_string_exn: unexpected character '%c'" c ()


let of_codons codons = 
  Array.map codons ~f:Codon.Universal_genetic_code.NS.to_string
  |> String.concat_array
  |> of_string_exn

let gc_contents s =
  let n = String.count s ~f:(function
      | 'C' | 'G' -> true
      | 'A' | 'T' -> false
      | _ -> assert false
    )
  in
  float n /. float (String.length s)
