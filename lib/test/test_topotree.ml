open Biocaml_phylogeny_core
open Alcotest

let mytree = TopoTree.Node (
    (0.135, TopoTree.Node (
        (0.11, TopoTree.Leaf 0),
        (0.18, TopoTree.Leaf 1))
    ),
    (0.23, TopoTree.Leaf 2)
  )

let test_of_string () =
  TopoTree.of_string "0.135;0.23;0.11;0.18;0;1;2" |>
  (check @@ testable TopoTree.pp (=)) "identical trees" mytree

let tests = [
  "of_string", `Quick, test_of_string
]
