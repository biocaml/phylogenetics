open Printf
open Core_kernel.Std
open Biocaml_phylogeny_core.Zipper


let rec zipper_explorer z =
  print z ;
  printf "Type B0, B1 or B2 to move, anything else to exit:" ;
  Out_channel.flush stdout ;
  match In_channel.input_line In_channel.stdin with
  | Some "B0" -> zipper_explorer (move z B0)
  | Some "B1" -> zipper_explorer (move z B1)
  | Some "B2" -> zipper_explorer (move z B2)
  | Some "print" -> zipper_explorer z
  | _ -> ()


let _ = zipper_explorer (zipper_of_tree (Biocaml_phylogeny_core.TopoTree.make_random 20))
