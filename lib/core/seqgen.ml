open Core_kernel.Std
open Sigs

module Make (E:EVOL_MODEL) =
struct
  include Model_utils.Make (E)

  let proba param base t =
    LATools.mat_vec_mul (eMt param t) (known_vector base)

  let draw_base vec =
    let open Base in
    (* for all base check if x is smaller than transition proba,
       if yes return base else decrement x *)
    let rec aux i x =
      let proba = LATools.get_vec vec i in
      if x < proba then of_int (i-1)
      else aux (i+1) (x-.proba)
    in
    Random.float 1.0 |> aux 1

  let seqgen_raw param tree size =
    let rec aux tree bl = match tree with
      | TopoTree.Leaf i -> [(i,bl)]
      | TopoTree.Node ((t1,l), (t2,r)) ->
        aux l (List.map bl ~f:(fun b->draw_base (proba param b t1)))
        @ aux r (List.map bl ~f:(fun b->draw_base (proba param b t2)))
    in
    List.init size ~f:(fun _->draw_base (stat_dist_vec param))
    |> aux tree

  let seqgen param tree size =
    seqgen_raw param tree size
    |> List.map ~f:(fun (i,s)->(i,Seq.of_list s))
    |> Align.of_assoc_list

  let seqgen_string_list param tree size =
    let raw = seqgen_raw param tree size in
    List.init (List.length raw) ~f:(
      fun i ->
        List.Assoc.find_exn raw i
        |> Seq.of_list |> Seq.to_string
    )
end
