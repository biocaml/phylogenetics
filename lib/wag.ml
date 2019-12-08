open Core_kernel

type t = {
  rate_matrix : Amino_acid.matrix ;
  freqs : Amino_acid.vector ;
}

let parse_aa_order l =
  let chars = String.filter l ~f:(function 'A'..'Y' -> true | _ -> false) in
  Amino_acid.Table.init (fun aa ->
      let aa = Amino_acid.to_char aa in
      match String.lfindi chars ~f:(fun _ c -> Char.equal c aa) with
      | Some i -> i
      | None -> failwith chars
    )

let parse_floats l =
  String.split ~on:' ' l
  |> List.filter ~f:(( <> ) "")
  |> List.map ~f:Float.of_string
  |> Array.of_list
  
let parse_freqs aa_order l =
  let values = parse_floats l in
  Amino_acid.Vector.init (fun aa -> values.(Amino_acid.Table.get aa_order aa))

let parse_rate_matrix aa_order lines =
  let values = Array.map lines ~f:parse_floats in
  Rate_matrix.Amino_acid.make (fun aa1 aa2 ->
      let i = Amino_acid.Table.get aa_order aa1 in
      let j = Amino_acid.Table.get aa_order aa2 in
      values.(max i j - 1).(min i j)
    )

let parse fn =
  let lines =
    In_channel.read_lines fn
    |> Array.of_list
  in
  let rate_matrix_lines = Array.sub lines ~pos:0 ~len:19 in
  let freqs_line = lines.(20) in
  let aa_line = lines.(23) in
  let aa_order = parse_aa_order aa_line in
  let freqs = parse_freqs aa_order freqs_line in
  let rate_matrix = parse_rate_matrix aa_order rate_matrix_lines in
  { rate_matrix ; freqs }
