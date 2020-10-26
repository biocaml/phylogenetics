open Core_kernel
open Phylogenetics
open Zipper
open Alcotest

let eps = 0.1

(** {6 Test input parameters} *)

(*/--0.200--/--0.400--<T0>
  |         \--0.300--<T1>
  \--0.100--<T2>*)
let mytree = Phylogenetic_tree.of_preorder "0.2;0.1;0.4;0.3;0;1;2"
let mybranches = Phylogenetic_tree.[of_preorder "0.4;0.3;0;1"; of_preorder "2"]

(*/--0.200--<T0>
  \--0.200--/--0.300--<T1>
            \--0.300--<T2>*)
let myothertree = Phylogenetic_tree.of_preorder "0.2;0.2;0;0.3;0.3;1;2"


(** {6 Test functions} *)

let test_of_tree_check_subtress () =
  of_tree mytree |> (fun z -> [get_tree z Dir0; get_tree z Dir1]) |>
  (check @@ slist Phylogenetic_tree.(testable pp equal) Poly.compare) "identical subtrees" mybranches

let test_of_tree_check_lengths () =
  of_tree mytree |> (fun z -> [get_length z Dir0; get_length z Dir1]) |>
  (check @@ slist (float eps) Poly.compare) "identical branch lengths" [0.1; 0.2]

let test_tree_and_back () =
  of_tree mytree |> to_tree |>
  Phylogenetic_tree.(check @@ testable pp equal) "identical trees" mytree

let test_of_tree_dir_move_and_back () =
  of_tree_dir mytree |> move_left |> unorient |> to_tree |>
  Phylogenetic_tree.(check @@ testable pp equal) "identical trees" myothertree

let test_goto () =
  let zipper_goto = of_tree mytree |> init_routing |> fun z -> goto z 2 in
  let zipper_move = of_tree mytree |> fun z -> move z Dir0 |> fun z -> move z Dir0 in
  Zipper.(check @@ testable pp equal) "identical zippers" zipper_move zipper_goto


(** {6 Test list} *)

let tests = [
  "of_tree (check subtrees)", `Quick, test_of_tree_check_subtress;
  "of_tree (check lengths)", `Quick, test_of_tree_check_lengths;
  "of_tree and back to_tree", `Quick, test_tree_and_back;
  "of_tree_dir, move_left and back to_tree", `Quick, test_of_tree_dir_move_and_back;
  "goto vs several moves", `Quick, test_goto
]
