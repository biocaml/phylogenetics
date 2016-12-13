%{

    (* http://evolution.genetics.washington.edu/phylip/newicktree.html *)
    open Printf
    open Newick

%}

%token <float> FLOAT
%token <string> IDENT
%token COMMA SEMICOLON LPAREN RPAREN
%token EOI EOL

%nonassoc LPAREN

%start tree
%type <Newick.tree> tree

%%

tree:
| LPAREN branches = separated_nonempty_list(COMMA, branch) RPAREN { Node branches }
;

branch:
|      { { id = None ; length = None ; tip = Node [] } }
| tree { { id = None ; length = None ; tip = $1 } }
;

