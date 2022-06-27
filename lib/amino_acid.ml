open Core
include Alphabet.Make(struct let card = 20 end)

let chars_of_aa_string = "ACDEFGHIKLMNPQRSTVWY"


let%test "chars_of_aa_string" =
  String.length chars_of_aa_string = card

let to_char i = chars_of_aa_string.[i]

let yojson_of_vector vec : Yojson.Safe.t =
  `Assoc (
    Vector.to_array vec
    |> Array.mapi ~f:(fun aa x -> to_char aa |> Char.to_string, `Float x)
    |> Array.to_list
  )

let vector_of_yojson aa_list =
  (* Optimization to validate that all AA are present *)
  let aa_queue =
    Yojson.Safe.Util.to_assoc aa_list
    |> List.sort ~compare:(fun (x, _) (y, _) -> String.compare x y)
    |> Queue.of_list in
  Vector.init (fun aa ->
      let aa_char = to_char aa in
      let is_expected aa_str = Char.(aa_str |> of_string = aa_char) in
      match Queue.dequeue aa_queue with
      | Some (key, x) when is_expected key -> Yojson.Safe.Util.to_number x
      | Some (key, _ ) ->
        failwithf "Expected value for %c ; found %s" aa_char key ()
      | None ->
        failwithf "Missing AA : %c ; reached end of json record without finding it"
          aa_char ()
    )

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
