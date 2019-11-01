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
