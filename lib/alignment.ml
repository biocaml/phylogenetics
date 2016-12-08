open Sigs

module Make (S:SEQUENCE) = struct
  type sequence = S.t
  type base = S.base
  type t = (int * S.t) list

  let get_base tab ~seq ~pos = S.get (List.assoc seq tab) pos

  let of_string_list l =
    let rec aux acc i = function
      | [] -> List.rev acc
      | s::t -> aux ((i, (S.of_string s))::acc) (i+1) t
    in aux [] 0 l

  let of_assoc_list l = l

  let pp fmt tab =
    List.map (function (x,y) -> Printf.sprintf "%d:%s" x (S.to_string y)) tab
    |> String.concat ";"
    |> Format.fprintf fmt "%s"
end
