(* #require "lacaml" *)
(* open Lacaml.S;; *)

(* let mat = Mat.random 4 4;; *)

open Biocaml_phylogeny ;;
open Alcotest ;;

let seq_test = (check string) "correct base from sequence" "C" (Sequence.test ()) ;;


(* TopoTree.test () ;; *)
(* Models.test () ;; *)
(* LATools.test () ;; *)
(* LATools.exptest () ;; *)
