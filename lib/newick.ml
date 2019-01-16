open Core_kernel

include Newick_ast

let parse buf =
  try Newick_parser.start Newick_lexer.token buf
  with Newick_parser.Error ->
    let pos = buf.lex_curr_p in
    failwithf "Incorrect newick format at %s:%d:%d" pos.pos_fname pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1) ()

let from_file fn =
  In_channel.with_file fn ~f:(fun ic ->
      parse (Lexing.from_channel ic)
    )

let from_string s =
  parse (Lexing.from_string s)
