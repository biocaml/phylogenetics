type tree = Node of branch list * int option
and branch = {
  id : string option ;
  length : float option ;
  tip : tree ;
} 
