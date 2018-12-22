type node_info = { name : string option }

type branch_info = {
  length : float option ;
  tags : tag list ;
}
and tag = string * string

type branch = (node_info, branch_info) Tree.branch
type tree = (node_info, branch_info) Tree.t

type t =
    Tree of tree
  | Branch of branch
