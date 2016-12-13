open Biocaml_phylogeny_core
open Alcotest

let f s =
  let buf = Lexing.from_string s in
  let ast = Newick_parser.tree Newick_lexer.token buf in
  ignore ast
