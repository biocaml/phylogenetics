open Phylogenetics
open Core_kernel
open Alcotest

let f s =
  let buf = Lexing.from_string s in
  let ast = Newick_parser.tree Newick_lexer.token buf in
  ast

let test () = f (In_channel.read_all "test_data/small1.tree")
