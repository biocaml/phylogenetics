{
open Lexing
open Newick_parser
}

rule token = parse
  | [' ''\t']+ { token lexbuf }
  | "\\\n" { token lexbuf }
  | '\n' { EOL }
  | ':' { COLON }
  | ';' { SEMICOLON }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | ',' { COMMA }

  | ['0'-'9']+ '.' ['0'-'9']* ('e' '-'? ['0'-'9']*)? as f
               { FLOAT (float_of_string f) }

  | ['0''1'] as i { BOOT (int_of_string (String.init 1 (fun _->i)) ) }

  | ['A'-'Z''a'-'z']['A'-'Z''a'-'z''0'-'9''-''_']* as lxm { IDENT(lxm) }

  | eof
      { EOI }

  | _
      { failwith (Printf.sprintf "At offset %d: unexpected character.\n" (Lexing.lexeme_start lexbuf)) }
