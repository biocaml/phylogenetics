open Biocaml_phylogeny_core
open Alcotest

let mytree = TopoTree.Node (
    (0.135, TopoTree.Node (
        (0.11, TopoTree.Leaf 0),
        (0.18, TopoTree.Leaf 1)
      )
    ),
    (0.23, TopoTree.Leaf 2)
  )

let test_of_preorder () =
  TopoTree.of_preorder "0.135;0.23;0.11;0.18;0;1;2" |>
  (check @@ testable TopoTree.pp (=)) "identical trees" mytree

let test_of_newick_file () =
  TopoTree.of_newick_file "test_data/small_2.tree" |>
  TopoTree.pp Format.std_formatter

let tests = [
  "of_preorder", `Quick, test_of_preorder
]
