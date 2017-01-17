open Biocaml_phylogeny_core.TopoTree
open Alcotest

let mytree = Node {
    left = 0.135, Node {
        left = 0.11, Leaf {index = "T0"; meta={id=0}};
        right = 0.18, Leaf {index = "T1"; meta={id=0}};
        meta = {id=0}
      };
    right = 0.23, Leaf {index = "T2"; meta={id=0}};
    meta = {id=0}
  }

let test_of_preorder () =
  of_preorder "0.135;0.23;0.11;0.18;0;1;2" |>
  (check @@ testable Biocaml_phylogeny_core.TopoTree.pp (=)) "identical trees" mytree

let tests = [
  "of_preorder", `Quick, test_of_preorder
]
