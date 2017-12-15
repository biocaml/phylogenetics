open Core_kernel

include Newick_types

let from_file fn =
  In_channel.with_file fn ~f:(fun ic ->
      let buf = Lexing.from_channel ic in
      Newick_parser.tree Newick_lexer.token buf
    )
