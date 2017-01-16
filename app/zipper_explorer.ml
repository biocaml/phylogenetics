open Printf
open Core_kernel.Std
open Biocaml_phylogeny_core.Zipper


let rec zipper_explorer z =
  print z ;
  printf "Type B0, B1 or B2 to move, type exit to exit:" ;
  Out_channel.flush stdout ;
  let dir_str = match In_channel.input_line In_channel.stdin with
    | Some s -> s | _ -> "exit"
  in match dir_str with
  | "B0" | "B1" | "B2" -> (
      try dir_of_string dir_str |> move z |> zipper_explorer with
      | Failure s -> printf "Encountered error: %s\n" s ; Out_channel.flush stdout ; zipper_explorer z
    )
  | "exit" -> ()
  | _ -> zipper_explorer z


let _ = zipper_explorer (zipper_of_tree (Biocaml_phylogeny_core.TopoTree.make_random 20))
