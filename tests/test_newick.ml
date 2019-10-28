open Phylogenetics

let tree1 = {|ADH1:0.11[&&NHX:S=human:E=1.1.1.1];|}
let tree2 = {|(((A:0.0707394[&&NHX:Condition=0],(B:0.082502[&&NHX:Condition=1:Transition=1],(C:0.0399704[&&NHX:Condition=0],(D:0.0130784[&&NHX:Condition=0],E:0.0126315[&&NHX:Condition=1:Transition=1]):0.0679028[&&NHX:Condition=0]):0.0355645[&&NHX:Condition=0]):0.0829847[&&NHX:Condition=0]):0.025651[&&NHX:Condition=0],F:0.00179799[&&NHX:Condition=0]):0.0487697[&&NHX:Condition=0],(G:0.0797879[&&NHX:Condition=0],(H:0.0338484[&&NHX:Condition=0],(I:0.0526082[&&NHX:Condition=1],J:0.0366882[&&NHX:Condition=1]):0.0553277[&&NHX:Condition=1:Transition=1]):0.00758827[&&NHX:Condition=0]):0.0913262[&&NHX:Condition=0]);|}

let just_parse tree () =
  ignore (Newick.from_string tree)

let tests = [
  "just_parse_test1", `Quick, just_parse tree1 ;
  "just_parse_test2", `Quick, just_parse tree2 ;
]
