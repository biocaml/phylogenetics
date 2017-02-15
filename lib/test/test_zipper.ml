open Core_kernel.Std
open Biocaml_phylogeny_core
open Zipper
open Alcotest


(** {6 Test input parameters} *)
(*
/--0.200--/--0.400--<T0>
|         \--0.300--<T1>
\--0.100--<T2>*)
let mytree = Phylogenetic_tree.of_preorder "0.2;0.1;0.4;0.3;0;1;2"
let mybranches = Phylogenetic_tree.[of_preorder "0.4;0.3;0;1"; of_preorder "2"]
(*
/--0.200--<T0>
\--0.200--/--0.300--<T1>
          \--0.300--<T2>*)
let myothertree = Phylogenetic_tree.of_preorder "0.2;0.2;0;0.3;0.3;1;2"

(** {6 Test functions} *)

let test_of_tree_check_subtress () =
  of_tree mytree |> (fun z -> [branch z Dir0; branch z Dir1] |> List.map ~f:(fun (_,x) -> x)) |>
  (check @@ slist (testable Phylogenetic_tree.pp (=)) compare) "identical subtrees" mybranches

let test_of_tree_check_lengths () =
  of_tree mytree |> (fun z -> [get_length z Dir0; get_length z Dir1]) |>
  (check @@ slist float compare) "identical branch lengths" [0.1; 0.2]

let test_tree_and_back () =
  of_tree mytree |> to_tree |>
  Phylogenetic_tree.(check @@ testable pp equal) "identical trees" mytree

let test_of_tree_dir_move_and_back () =
  of_tree_dir mytree |> move_left |> unorient |> to_tree |>
  Phylogenetic_tree.(check @@ testable pp equal) "identical trees" myothertree


(** {6 Test list} *)

let tests = [
  "of_tree (check subtrees)", `Quick, test_of_tree_check_subtress;
  "of_tree (check lengths)", `Quick, test_of_tree_check_lengths;
  "of_tree and back to_tree", `Quick, test_tree_and_back;
  "of_tree_dir, move_left and back to_tree", `Quick, test_of_tree_dir_move_and_back
]
