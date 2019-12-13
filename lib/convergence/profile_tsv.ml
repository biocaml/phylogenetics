open Core_kernel

let read fn =
  In_channel.read_lines fn
  |> List.map ~f:(String.split ~on:'\t')
  |> List.map ~f:(List.map ~f:Float.of_string)
  |> List.map ~f:Array.of_list
  |> Array.of_list
  |> Array.transpose_exn

let to_fitness ?(beta = 1.) =
  Array.map ~f:(Array.map ~f:(fun x -> beta *. Float.log x))
