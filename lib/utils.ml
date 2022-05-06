open Core

let marker = function
  | "" -> "\027[0m"
  | "red" -> "\027[31m"
  | "green" -> "\027[32m"
  | "yellow" -> "\027[33m"
  | "blue" -> "\027[34m"
  | "magenta" -> "\027[35m"
  | "cyan" -> "\027[36m"
  | s -> failwith (sprintf "Unrecognized marker %s." s)

let rec insert_colors str =
  match String.lsplit2 ~on:'$' str with
  | None -> str
  | Some (beg, en) -> beg ^ (match String.lsplit2_exn ~on:'$' en with
      | m, en2 -> (marker m) ^ (insert_colors en2)
    )

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

let colorize color to_colorize string =
  String.concat_map string ~f:(fun c ->
      if String.contains to_colorize c
      then fancy_sprintf "%s%c$$" (marker color) c
      else sprintf "%c" c)

let apply_options options s =
  List.fold options ~init:s ~f:(fun s f -> f s)

let print f = fun s -> f s |> defancy |> printf "%s"

let print_fancy ?(options=[]) f = fun s -> f s |> apply_options options |> printf "%s"

let pp f = fun fmt s -> f s |> defancy |> Format.fprintf fmt "%s"

let pp_fancy ?(options=[]) f = fun fmt s -> f s |> apply_options options |> Format.fprintf fmt "%s"

let all_printers ?(options=[]) f = pp f, pp_fancy ~options f, print f, print_fancy ~options f

type float_array = float array
[@@deriving show]

type float_array_array = float array array
[@@deriving show]

let robust_equal x y =
  Float.robustly_compare x y = 0

let float_array_robust_equal x y =
  let res = Array.for_alli x ~f:(fun i _ -> robust_equal x.(i) y.(i)) in
  if not res then (
    fprintf stderr "expected: %s\ngot: %s\n" (show_float_array x) (show_float_array y)
  );
  res

let random_profile rng n =
  let v = Array.init n ~f:(fun _ -> Gsl.Rng.uniform rng) in
  let s = Array.fold v ~init:0. ~f:( +. ) in
  Linear_algebra.Vector.init n ~f:(fun i -> v.(i) /. s)

let array_sum xs = Array.fold xs ~f:( +. ) ~init:0.

let array_order xs ~compare =
  let ys = Array.mapi xs ~f:(fun i x -> x, i) in
  Array.sort ys ~compare:(Tuple2.compare ~cmp1:compare ~cmp2:Int.compare) ;
  Array.map ~f:snd ys

let rng_of_int seed =
  let res = Gsl.Rng.(make (default ())) in
  Gsl.Rng.set res (Nativeint.of_int seed) ;
  res
