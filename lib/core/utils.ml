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
