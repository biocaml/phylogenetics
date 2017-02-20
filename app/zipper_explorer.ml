open Printf
open Core_kernel.Std
open Biocaml_phylogeny_core.Zipper

let zipper_explorer z =
  let rec get_command () =
    match In_channel.input_line In_channel.stdin with
    | Some s -> s
    | _ -> failwith "No command."
  and parse_command s z =
    match String.strip s |> String.split ~on:' ' with
    | ["move"; s] -> dir_of_string s |> move z |> display
    | ["slide"; sd; sf] -> slide z (dir_of_string sd) (float_of_string sf) |> display
    | ["goto"; si] -> goto z (int_of_string si) |> display
    | ["random_node"] -> random_node z |> display
    | ["exit"] -> ()
    | _ -> help z
  and help z = printf "Available commands:\n* move dir\n* slide dir len\n* goto int\n* random_node\n* exit\n" ; prompt z
  and display z = print_fancy z ; flush_all () ; prompt z
  and prompt z = printf "Type a command: " ; flush_all () ;
    let c = get_command () in
    try parse_command c z with
    | Failure s -> printf "Error: %s\n" s ; help z
    | _ -> printf "Unknown error.\n" ; help z
  in display z

let _ = Biocaml_phylogeny_core.Phylogenetic_tree.make_random 20
        |> of_tree
        |> init_routing
        |> zipper_explorer
