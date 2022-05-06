open Core

module type Base = sig
  type t
  val to_char : t -> char
  val of_char_exn : char -> t
end

module type S = sig
  type base
  type t
  val get : t -> int -> base
  val length : t -> int
  val of_list : base list -> t
  val of_string_exn : string -> t
  val to_string : t -> string
  val pp : Format.formatter -> t -> unit
end

module Make(B : Base) = struct
  type base = B.t
  type t = {length:int ; array:base array}

  let get seq i = Array.get seq.array i

  let length seq = seq.length

  let of_string_exn str = {
    length = String.length str ;
    array = Array.init (String.length str) ~f:(
      fun i -> B.of_char_exn (str.[i])
    )
  }

  let of_list l = {
    length = List.length l ;
    array = Array.init (List.length l) ~f:(
      fun i -> List.nth_exn l i
    )
  }

  let to_string seq =
    String.init (length seq) ~f:(
      fun i -> B.to_char (get seq i)
    )

  let pp fmt seq =
    to_string seq
    |> Format.fprintf fmt "%s"
end

module Make_list (B : Base) = struct
  type base = B.t
  type t = base list

  let get seq i = match List.nth seq i with Some b->b | None->failwith "Base not found"

  let length seq = List.length seq

  let of_string_exn str =
    let rec aux i acc =
      if (i >= String.length str) then
        List.rev acc
      else
        match B.of_char_exn str.[i] with
        | b -> aux (i+1) (b::acc)
        | exception _ -> invalid_arg "input string"
    in
    aux 0 []

  let of_list l = l (* wow *)

  let to_string seq =
    List.map ~f:B.to_char seq
    |> String.of_char_list

  let pp fmt seq = to_string seq |> Format.fprintf fmt "%s"
end

module DNA = Make(Nucleotide)
