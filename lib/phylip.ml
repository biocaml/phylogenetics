(*
http://evolution.genetics.washington.edu/phylip/doc/sequence.html
http://scikit-bio.org/docs/0.2.3/generated/skbio.io.phylip.html
*)
open Core
open Rresult

type item = {
  name : string ;
  sequence : string ;
}

type t = {
  number_of_sequences : int ;
  sequence_length : int ;
  items : item list ;
}

let make_exn = function
  | [] -> invalid_arg "empty list of items"
  | h :: t as items ->
    let n = String.length h.sequence in
    (
      match List.findi t ~f:(fun _ it -> String.length it.sequence <> n) with
      | Some (i, it) -> invalid_argf "Sequence %d has length %d while it is expected to have length %d" i (String.length it.sequence) n ()
      | None -> ()
    ) ;
    { number_of_sequences = List.length items ; sequence_length = n ; items }

module Relaxed_parser = struct
  let parse_header l =
    match
      String.split_on_chars l ~on:['\t' ; ' ']
      |> List.filter ~f:(String.( <> ) "")
    with
    | [ m ; n ] -> (
        try Ok (Int.of_string m, Int.of_string n)
        with _ -> Error (`Msg "Incorrect header")
      )
    | _ -> Error (`Msg "Incorrect header")

  let check_nb_lines lines number_of_sequences =
    if List.length lines = number_of_sequences then Ok ()
    else Error (`Msg "Unexpected number of lines in file")

  let parse_item ~sequence_length i l =
    let err () = Error (`Msg (sprintf "incorrect sequence syntax on line %d" (i + 1))) in
    match String.lsplit2 l ~on:'\t' with
    | Some (name, sequence) ->
      if String.length sequence = sequence_length then Ok { name ; sequence }
      else err ()
    | None -> err ()

  let read fn =
    let open Result.Monad_infix in
    match In_channel.read_lines fn with
    | [] -> Error (`Msg "Empty file")
    | header :: sequences ->
      parse_header header >>= fun (number_of_sequences, sequence_length) ->
      check_nb_lines sequences number_of_sequences >>= fun () ->
      List.mapi sequences ~f:(parse_item ~sequence_length) |> Result.all >>= fun items ->
      Ok { number_of_sequences ; sequence_length ; items }
end

let make ~number_of_sequences ~sequence_length ~items =
  let nseq = List.length items in
  if nseq <> number_of_sequences then
    R.error_msgf "Declared %d sequences but provided %d" number_of_sequences nseq
  else if List.exists items ~f:(fun { sequence = s ; _ } -> String.length s <> sequence_length) then
    R.error_msgf "Not all sequences have declared length of %d" sequence_length
  else Ok {
      number_of_sequences ;
      sequence_length ;
      items ;
    }

module Parser = struct
  open Angstrom

  let is_space = function
    | ' ' -> true
    | _ -> false

  let space = skip_while is_space <?> "space"
  let space1 = (satisfy is_space *> space) <?> "space1"

  let integer =
    (
      take_while1 (
        function
        | '0'..'9' -> true
        | _ -> false
      ) >>= fun s ->
      try return (Int.of_string s)
      with Failure msg -> fail msg
    ) <?> "integer"

  let header_parser =
    (
      space  *> integer >>= fun number_of_sequences ->
      space1 *> integer >>= fun sequence_length ->
      space  *> char '\n' >>= fun _ ->
      return (number_of_sequences, sequence_length)
    ) <?> "header_parser"

  let id_parser =
    count 10 (not_char '\n') >>| String.of_char_list >>| Stdlib.String.trim

  let sequence_parser =
    take_while1 (function
        | 'A'..'Z' | 'a'..'z' | '-' | '.' -> true
        | _ -> false
      )

  let item =
    id_parser >>= fun name ->
    sequence_parser >>= fun sequence ->
    return { name ; sequence }

  let file =
    header_parser >>= fun (number_of_sequences, sequence_length) ->
    sep_by1 (char '\n') item >>= fun items ->
    match make ~number_of_sequences ~items ~sequence_length with
    | Ok x -> return x
    | Error (`Msg m) -> fail m
end

let read ?(strict = true) fn =
  if strict then
    In_channel.with_file fn ~f:(fun ic ->
        Angstrom_unix.parse Parser.file ic
      )
    |> snd
    |> Result.map_error ~f:(fun s -> `Msg s)
  else
    Relaxed_parser.read fn

let read_exn ?strict fn =
  match read ?strict fn with
  | Ok r -> r
  | Error (`Msg msg) -> failwith msg

let write_strict data fn =
  Out_channel.with_file fn ~f:(fun oc ->
      fprintf oc "%d %d\n" data.number_of_sequences data.sequence_length ;
      List.iter data.items ~f:(fun it ->
          let id =
            let n = String.length it.name in
            if n <= 10 then (it.name ^ String.make (10 - n) ' ')
            else String.prefix it.name 10
          in
          fprintf oc "%s%s\n" id it.sequence
        )
    )

let write_relaxed data fn =
  Out_channel.with_file fn ~f:(fun oc ->
      fprintf oc "%d\t%d\n" data.number_of_sequences data.sequence_length ;
      List.iter data.items ~f:(fun it ->
          fprintf oc "%s\t%s\n" it.name it.sequence
        )
    )

let write ?(strict = true) t fn =
  if strict then write_strict t fn
  else write_relaxed t fn
