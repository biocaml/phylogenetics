%{
    (* http://evolution.genetics.washington.edu/phylip/newicktree.html *)
    let list_of_opt = Base.Option.value ~default:[]
%}

%token <string> FLOAT
%token <string> INT
%token <string> STRING
%token COMMA COLON SEMICOLON LPAREN RPAREN LBRACKET RBRACKET NHXTAG EQUAL EOF

%start start
%type <Newick_ast.t> start

%%

start: node SEMICOLON EOF {$1};

node:
  | name = option(STRING) parent_branch = option(length) tags = option(tags)
  { Newick_ast.{name ; tags = list_of_opt tags ; parent_branch ; children = []}
  }
  | LPAREN children = separated_nonempty_list(COMMA, node) RPAREN name = option(STRING) parent_branch = option(length) tags = option(tags)
  { Newick_ast.{name ; tags = list_of_opt tags ; parent_branch ; children }
  }
;

length:
  | COLON l = FLOAT { float_of_string l }
  | COLON l = INT { float_of_string l }
;

tags:
  | LBRACKET NHXTAG tags = nonempty_list(tag) RBRACKET { tags }
;

tag:
  | COLON key = STRING EQUAL data = STRING { (key, data) }
  | COLON key = STRING EQUAL data = FLOAT { (key, data) }
  | COLON key = STRING EQUAL data = INT { (key, data) }
;
