type node_info = {
  name : string option ;
}

and branch_info = {
  length : float option ;
  tags : tag list ;
}

and tag = string * string

type tree = (node_info, node_info, branch_info) Tree.t
type branch = (node_info, node_info, branch_info) Tree.branch

type t =
  | Tree of tree
  | Branch of branch

type error_desc = {
  offset : int ;
  line : int ;
  column : int ;
  msg : string ;
}

let string_of_error_desc e =
  Printf.sprintf "Error at line %d, column %d: %s" e.line e.column e.msg

type error = [`Newick_parser_error of error_desc]

let mkerror lexbuf msg =
  let pos = Lexing.lexeme_start_p lexbuf in
  let line = pos.pos_lnum in
  let column = pos.pos_cnum - pos.pos_bol + 1 in
  let offset = pos.pos_cnum in
  `Newick_parser_error { offset ; line ; column ; msg }

let string_of_error (`Newick_parser_error ed) = string_of_error_desc ed
