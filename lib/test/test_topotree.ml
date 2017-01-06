open Biocaml_phylogeny_core
open Alcotest

let mytree = TopoTree.Node (
    (0.135, TopoTree.Node (
        (0.11, TopoTree.Leaf "T0"),
        (0.18, TopoTree.Leaf "T1")
      )
    ),
    (0.23, TopoTree.Leaf "T2")
  )

let test_of_preorder () =
  TopoTree.of_preorder "0.135;0.23;0.11;0.18;0;1;2" |>
  (check @@ testable TopoTree.pp (=)) "identical trees" mytree

let tests = [
  "of_preorder", `Quick, test_of_preorder
]
