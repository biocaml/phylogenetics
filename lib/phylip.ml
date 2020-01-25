open Core_kernel

type item = {
  name : string ;
  sequence : string ;
}

type t = {
  nb_sequences : int ;
  nb_cols : int ;
  items : item array ;
}

let parse_header l =
  match
    String.split l ~on:' '
    |> List.filter ~f:(String.( <> ) "")
  with
  | [ m ; n ] -> (
      try Ok (Int.of_string m, Int.of_string n)
      with _ -> Error (`Msg "Incorrect header")
    )
  | _ -> Error (`Msg "Incorrect header")

let check_nb_lines lines nb_sequences =
  if List.length lines = nb_sequences then Ok ()
  else Error (`Msg "Unexpected number of lines in file")

let parse_item ~nb_cols i l =
  let err () = Error (`Msg (sprintf "incorrect sequence syntax on line %d" (i + 1))) in
  match String.lsplit2 l ~on:'\t' with
  | Some (name, sequence) ->
    if String.length sequence = nb_cols then Ok { name ; sequence }
    else err ()
  | None -> err ()

let of_file fn =
  let open Result.Monad_infix in
  match In_channel.read_lines fn with
  | [] -> Error (`Msg "Empty file")
  | header :: sequences ->
    parse_header header >>= fun (nb_sequences, nb_cols) ->
    check_nb_lines sequences nb_sequences >>= fun () ->
    List.mapi sequences ~f:(parse_item ~nb_cols) |> Result.all >>= fun items ->
    let items = Array.of_list items in
    Ok { nb_sequences ; nb_cols ; items }
