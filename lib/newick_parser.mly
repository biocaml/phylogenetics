%{
    (* http://evolution.genetics.washington.edu/phylip/newicktree.html *)
    open Newick_types
    let list_of_opt = Base.Option.value ~default:[]
%}

%token <float> FLOAT
%token <string> STRING
%token COMMA COLON SEMICOLON LPAREN RPAREN LBRACKET RBRACKET NHXTAG EQUAL

%start start
%type <Newick_types.data> start

%%
start:
  | tree SEMICOLON { Tree $1 }
  | branch_with_length SEMICOLON { Branch $1 }
;

tree:
  | name = option(STRING)
    { Tree.{ branches = [] ; node_data = { name } } }
  | LPAREN branches = separated_nonempty_list(COMMA, branch) RPAREN name = option(STRING)
    { Tree.{ branches ; node_data = { name } } }

branch:
  | tip = tree length = option(length) tags = option(tags)
    {{ tip ; branch_data = { length ; tags = list_of_opt tags }}}
;

branch_with_length:
  | tip = tree l = length tags = option(tags)
    {{ tip ; branch_data = { length = Some l ; tags = list_of_opt tags } }}
;

length: COLON l = FLOAT { l }
;

tags:
| LBRACKET NHXTAG COLON tags = separated_nonempty_list(COLON, tag) RBRACKET { tags }
;

tag:
| key = STRING EQUAL data = STRING { (key, data) }
;
