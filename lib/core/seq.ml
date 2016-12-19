open Sigs
open Core_kernel.Std

module Make (B:BASE) = struct
  type base = B.t
  type t = base list

  let get seq i = match List.nth seq i with Some b->b | None->failwith "Base not found"

  let length seq = List.length seq

  let of_string str =
    let rec aux i acc =
      if (i >= String.length str) then
        List.rev acc
      else
        match B.of_char str.[i] with
        | b -> aux (i+1) (b::acc)
        | exception _ -> invalid_arg "input string"
    in
    aux 0 []

  let of_list l = l (* wow *)

  let to_string seq = List.map ~f:B.to_string seq |> String.concat

  let pp fmt seq = to_string seq |> Format.fprintf fmt "%s"
end

module DNA = Make (Nucleotide)
