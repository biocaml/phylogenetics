%{
    (* http://evolution.genetics.washington.edu/phylip/newicktree.html *)
    open Newick_ast
    let list_of_opt = Base.Option.value ~default:[]
%}

%token <string> FLOAT
%token <string> INT
%token <string> STRING
%token COMMA COLON SEMICOLON LPAREN RPAREN LBRACKET RBRACKET NHXTAG EQUAL EOF

%start start
%type <Newick_ast.t> start

%%
start:
  | tree SEMICOLON EOF { Tree $1 }
  | branch_with_length SEMICOLON EOF { Branch $1 }
;

tree:
  | name = option(STRING)
    { Tree.leaf { name } }
  | LPAREN branches = separated_nonempty_list(COMMA, branch) RPAREN name = option(STRING)
    { match branches with
      | [] -> assert false
      | h :: t -> Tree.node { name } (List1.cons h t) }

branch:
  | tip = tree length = option(length) tags = option(tags)
    { Tree.branch { length ; tags = list_of_opt tags } tip }
;

branch_with_length:
  | tip = tree l = length tags = option(tags)
    { Tree.branch { length = Some l ; tags = list_of_opt tags } tip }
;

length:
  | COLON l = FLOAT { float_of_string l }
  | COLON l = INT { float_of_string l }
;

tags:
| LBRACKET NHXTAG COLON tags = separated_nonempty_list(COLON, tag) RBRACKET { tags }
;

tag:
| key = STRING EQUAL data = STRING { (key, data) }
| key = STRING EQUAL data = FLOAT { (key, data) }
| key = STRING EQUAL data = INT { (key, data) }
;
