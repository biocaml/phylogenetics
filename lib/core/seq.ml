open Sigs

module Make (B:BASE) = struct
  type base = B.t
  type t = base list

  let get seq i = List.nth seq i

  let of_string str =
    let rec aux i acc =
      if (i >= String.length str) then
        List.rev acc
      else
        match B.of_char str.[i] with
        | b -> aux (i+1) (b::acc)
        | exception e -> invalid_arg "input string"
    in
    aux 0 []

  let of_list l = l (* wow *)

  let to_string seq = List.map B.to_string seq |> String.concat ""

  let pp fmt seq = to_string seq |> Format.fprintf fmt "%s"
end

module DNA = Make (Nucleotide)
