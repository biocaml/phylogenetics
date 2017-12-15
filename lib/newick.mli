(* type tree = Newick_types.tree = Node of branch list * int option *)
(* and branch = Newick_types.branch = { *)
(*   id : string option ; *)
(*   length : float option ; *)
(*   tip : tree ; *)
(* } *)
include module type of Newick_types

val from_file : string -> tree
