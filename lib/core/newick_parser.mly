%{
    (* http://evolution.genetics.washington.edu/phylip/newicktree.html *)
    open Printf
    open Newick
%}

%token <float> FLOAT
%token <string> IDENT
%token <int> BOOT
%token COMMA COLON SEMICOLON LPAREN RPAREN
%token EOI EOL

%start tree
%type <Newick.tree> tree

%%
tree:
| itree SEMICOLON { $1 }

itree:
| LPAREN branches = separated_nonempty_list(COMMA, branch) RPAREN i = BOOT { Node (branches, Some i) }
| LPAREN branches = separated_nonempty_list(COMMA, branch) RPAREN { Node (branches, None) }
;

branch:
| IDENT COLON FLOAT {{ id = Some $1 ; length = Some $3 ; tip = Node ([], None) }}
| itree COLON FLOAT {{ id = None ; length = Some $3 ; tip = $1 }}
;
