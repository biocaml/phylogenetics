type tree = Node of {
    children : branch list ;
    name : string option ;
  }

and branch = {
  length : float option ;
  tip : tree ;
  tags : tag list
}

and tag = string * string

and data = Tree of tree | Branch of branch
