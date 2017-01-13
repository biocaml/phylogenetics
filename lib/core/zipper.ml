open Printf
open TopoTree

type branch = float * TopoTree.t
type zipper =
  | InNode of {b0:branch; b1:branch; b2:branch}


(* ================== *)
(*  ZIPPER FUNCTIONS  *)
(* ================== *)

let zipper_of_tree = function
  | Node ( (l1,Node((l2,t1),(l3,t2))), (l4,t3)) |
    Node ((l4,t3), (l1,Node((l2,t1),(l3,t2))) ) ->
    InNode {b0=l1+.l4, t3; b1=l2, t1; b2=l3, t2}
  | _ -> failwith "Zipper cannot be positioned at a leaf."

let tree_of_zipper = function
  | InNode {b0=l1,t1; b1=l2,t2; b2=l3,t3} ->
    Node ( (l1/.2., t1), (l1/.2., Node ((l2, t2), (l3, t3))))

let branch z i = match i, z with
  | 0, InNode {b0=x;_} |
    1, InNode {b1=x;_} |
    2, InNode {b2=x;_} -> x
  | i, _ -> failwith (sprintf "Invalid branch index %d. Index must be between 0 and 2." i)

let move z i = match i, z with
  | 0, InNode {b0=l,Node (x,y); b1=a; b2=b} |
    1, InNode {b1=l,Node (x,y); b0=a; b2=b} |
    2, InNode {b2=l,Node (x,y); b0=a; b1=b} -> InNode {b0=x; b1=y; b2=l,Node(a,b)}
  | i, _ when i>2 -> failwith (sprintf "Invalid branch index %d. Index must be between 0 and 2." i)
  | _ -> failwith "Cannot go in this direction because it is a leaf."
