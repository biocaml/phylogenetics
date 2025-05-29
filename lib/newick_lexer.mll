{
open Newick_parser

exception Error of Newick_ast.error
}

rule token = parse
  | [' ''\t''\n''\r']+ { token lexbuf }
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
  | ['0'-'9']+ ('.' ['0'-'9']*)? (['e''E'] ['+' '-']? ['0'-'9']*)? as f
               { FLOAT f }

  | ['A'-'Z''a'-'z''0'-'9''-''_''.''/']+ as lxm { STRING(lxm) }

  | _ as c
      { raise (Error (Newick_ast.mkerror lexbuf (Printf.sprintf "unexpected character: %c" c))) }
