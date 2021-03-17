{
open Newick_parser

exception Error of Newick_ast.error
}

rule token = parse
  | [' ''\t']+ { token lexbuf }
  | "\\\n" { token lexbuf }
  | ':' { COLON }
  | ';' { SEMICOLON }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | ',' { COMMA }
  | '[' { LBRACKET }
  | ']' { RBRACKET }
  | "&&NHX" { NHXTAG }
  | '=' { EQUAL }
  | eof { EOF }

  | ['0'-'9']+ as i
               { INT i }
  | ['0'-'9']+ ('.' ['0'-'9']*)? ('e' '-'? ['0'-'9']*)? as f
               { FLOAT f }

  | ['A'-'Z''a'-'z''0'-'9''-''_''.''/']+ as lxm { STRING(lxm) }

  | _
      { raise (Error (Newick_ast.mkerror lexbuf "unexpected character")) }
