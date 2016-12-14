open Biocaml_phylogeny_core
open Alcotest

let f s =
  let buf = Lexing.from_string s in
  let ast = Newick_parser.tree Newick_lexer.token buf in
  ast

let test () = f "(T4:0.121186,(T3:0.109168,(T2:0.594785,(T1:0.127129,T0:0.11626)1:0.273997)1:0.034558)1:0.0530378);"
