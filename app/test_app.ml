(* #require "lacaml" *)
(* open Lacaml.S;; *)

(* let mat = Mat.random 4 4;; *)

open Biocaml_phylogeny ;;

TopoTree.test () ;;
Models.test () ;;
LaTools.test () ;;
LaTools.exptest () ;;
