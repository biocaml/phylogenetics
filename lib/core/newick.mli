type tree = Node of branch list
and branch = {
  id : string option ;
  length : float option ;
  tip : tree ;
}

