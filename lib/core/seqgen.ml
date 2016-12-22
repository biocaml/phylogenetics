open Sigs
open Core_kernel.Std

module Seqgen (E:EVOL_MODEL) =
struct
  module Utils = Model_utils.Model_utils (E)
  include Utils
  open LATools

  let proba param base t =
    mat_vec_mul (eMt param t) (known_vector base)

  let draw_base vec =
    let open Base in
    (* for all base check if x is smaller than transition proba,
       if yes return base else decrement x *)
    let rec aux i x =
      let proba = get_vec vec i in
      if x < proba then of_int (i-1)
      else aux (i+1) (x-.proba)
    in
    Random.float 1.0 |> aux 1

  let seqgen param tree size =
    let rec aux tree base = match tree with
      | TopoTree.Leaf i -> [(i, [base])]
      | TopoTree.Node ((t1,l), (t2,r)) ->
        (aux l (draw_base (proba param base t1)))
        @ (aux r (draw_base (proba param base t2)))
    in let merge l1 l2 =
         List.map l1 ~f:(fun (i,seq) -> (i,(List.Assoc.find_exn l2 i)@seq))
    in
    List.range 1 size
    |> List.map ~f:(fun _->draw_base (stat_dis_vec param) |> aux tree)
    |> List.reduce_exn ~f:merge
    |> List.map ~f:(fun (i,seq)->(i, Seq.of_list seq))
    |> Align.of_assoc_list

end

module JCSeqgen = Seqgen (Models.JC69)

let test () =
  JCSeqgen.draw_base (JCSeqgen.proba () (JCSeqgen.Base.of_int 0) 0.7)
  |> JCSeqgen.Base.to_string

let test2 () =
  let mytree = TopoTree.of_newick_file "test_data/small_1.tree" in
  JCSeqgen.seqgen () mytree 5
  |> JCSeqgen.Align.pp Format.std_formatter
