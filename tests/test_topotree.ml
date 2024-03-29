open Core
open Phylogenetics.Phylogenetic_tree
open Alcotest


(** {6 Test input parameters} *)

let hs = Hashtbl.hash

let mytree = Node {
    left = 0.135, Node {
        left = 0.11, Leaf {index = "T0"; meta={routing_no= -1; id=hs "T0"}};
        right = 0.18, Leaf {index = "T1"; meta={routing_no= -1; id=hs "T1"}};
        meta = {routing_no= -1; id=hs (hs "T0" + hs "T1")}
      };
    right = 0.23, Leaf {index = "T2"; meta={routing_no= -1; id=hs "T2"}};
    meta = {routing_no= -1; id=hs (hs "T2" + hs (hs "T0" + hs "T1"))}
  }


(** {6 Test functions} *)

let test_of_preorder () =
  of_preorder "0.135;0.23;0.11;0.18;0;1;2" |>
  (check @@ testable Phylogenetics.Phylogenetic_tree.pp Poly.equal) "identical trees" mytree


(** {6 Test list} *)

let tests = [
  "of_preorder", `Quick, test_of_preorder
]
