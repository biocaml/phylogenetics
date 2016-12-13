{
  open Lexing
  open Newick_parser

}

rule token = parse
| [' ''\t']+ { token lexbuf }
| "\\\n" { token lexbuf }
| '\n' { EOL }
| ';' { SEMICOLON }
| '(' { LPAREN }
| ')' { RPAREN }
| ',' { COMMA }

| ['0'-'9']+ '.' ['0'-'9']* as f
    { FLOAT (float_of_string f) }

| ['A'-'Z''a'-'z''0'-'9''_']['A'-'Z''a'-'z''0'-'9''-''_']* as lxm { IDENT(lxm) }

| eof
    { EOI }

| _
    { failwith (Printf.sprintf "At offset %d: unexpected character.\n" (Lexing.lexeme_start lexbuf)) }










