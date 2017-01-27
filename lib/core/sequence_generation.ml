open Core_kernel.Std
open Sigs

module Make (E:EVOL_MODEL) =
struct
  include E

  let proba param =
    let my_eMt = eMt_mat param in
    fun base t -> Linear_algebra_tools.mat_vec_mul (my_eMt t) (known_vector base)

  let draw_base vec =
    let open Base in
    (* for all base check if x is smaller than transition proba,
       if yes return base else decrement x *)
    let rec aux i x =
      let proba = Linear_algebra_tools.get_vec vec i in
      if x < proba then of_int (i-1)
      else aux (i+1) (x-.proba)
    in
    Random.float 1.0 |> aux 1

  let seqgen_raw param =
    let my_proba = proba param in
    fun tree size ->
      let rec aux tree bl = match tree with
        | TopoTree.Leaf {index=i; _} -> [(i,bl)]
        | TopoTree.Node {left=t1,l; right=t2,r; _} ->
          aux l (List.map bl ~f:(fun b->draw_base (my_proba b t1)))
          @ aux r (List.map bl ~f:(fun b->draw_base (my_proba b t2)))
      in
      List.init size ~f:(fun _->draw_base (stat_dist_vec param))
      |> aux tree

  let seqgen param =
    let my_seqgen = seqgen_raw param in
    fun tree size ->
      my_seqgen tree size
      |> List.map ~f:(fun (i,s)->(i,Seq.of_list s))
      |> Align.of_assoc_list

  let seqgen_string_list param =
    let my_seqgen = seqgen_raw param in
    fun tree size ->
      let raw = my_seqgen tree size in
      List.init (List.length raw) ~f:(
        fun i ->
          List.Assoc.find_exn raw (TopoTree.index_of_int i)
          |> Seq.of_list |> Seq.to_string
      )
end
