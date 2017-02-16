open Core_kernel.Std
open Printf

let dim to_dim string =
  String.concat_map string ~f:(fun c ->
      if String.contains to_dim c
      then sprintf "\027[2m%c\027[0m" c
      else sprintf "%c" c)

let apply_options options s =
  List.fold options ~init:s ~f:(fun s f -> f s)

let print ?(options=[]) f = fun s -> f s |> apply_options options |> printf "%s"

let pp ?(options=[]) f = fun fmt s -> f s |> apply_options options |> Format.fprintf fmt "%s"



let rec insert_colors str =
  match String.lsplit2 ~on:'$' str with
  | None -> str
  | Some (beg, en) -> beg ^ (match String.lsplit2_exn ~on:'$' en with
      | m, en2 -> (marker m) ^ (insert_colors en2)
    )
and marker = function
  | "" -> "\027[0m"
  | "red" -> "\027[31m"
  | "green" -> "\027[32m"
  | "yellow" -> "\027[33m"
  | "blue" -> "\027[34m"
  | "magenta" -> "\027[35m"
  | "cyan" -> "\027[36m"
  | _ -> failwith "Unrecognized marker."

let rec defancy str =
  match String.lsplit2 ~on:'\027' str with
  | None -> str
  | Some (beg, en) ->
    beg ^ (match String.lsplit2_exn ~on:'m' en with _, en2 -> defancy en2)

let fancy_length str = defancy str |> String.length

let fancy_format format =
  Scanf.format_from_string
    (insert_colors (string_of_format format))
    format

(** sprintf variant that recognizes markers for colored output.*)
let fancy_sprintf format = sprintf (fancy_format format)


let test () = let mystr = fancy_sprintf "$cyan$%.3f$$" 1.0 in
  fancy_length mystr, String.length "1.000"
