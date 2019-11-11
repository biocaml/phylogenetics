open Core_kernel
open Sigs

module type Base = sig
  type t
  val to_int : t -> int
  val of_int_exn : int -> t
end

module type Sequence = sig
  type t
  type base
  val to_string: t -> string
  val of_list: base list -> t
end

module Make
    (Base : Base)
    (Seq : Sequence with type base = Base.t)
    (Align : ALIGNMENT with type sequence = Seq.t)
    (E : EVOL_MODEL with type base := Base.t) =
struct
  open Linear_algebra

  let proba param =
    let my_eMt = E.eMt_mat param in
    fun base t -> Mat.row (my_eMt t) (Base.to_int base)

  let draw_base vec =
    (* for all base check if x is smaller than transition proba,
       if yes return base else decrement x *)
    let rec aux i x =
      let proba = Vec.get vec i in
      if x < proba then Base.of_int_exn i
      else aux (i+1) (x-.proba)
    in
    Random.float 1.0 |> aux 0

  let seqgen_raw param =
    let my_proba = proba param in
    let stat_dist = E.stat_dist_vec param in
    fun tree size ->
      let rec aux tree bl = match tree with
        | Phylogenetic_tree.Leaf {index=i; _} -> [(i,bl)]
        | Phylogenetic_tree.Node {left=t1,l; right=t2,r; _} ->
          aux l (List.map bl ~f:(fun b->draw_base (my_proba b t1)))
          @ aux r (List.map bl ~f:(fun b->draw_base (my_proba b t2)))
      in
      List.init size ~f:(fun _->draw_base stat_dist)
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
          List.Assoc.find_exn ~equal:String.( = ) raw (Phylogenetic_tree.index_of_int i)
          |> Seq.of_list |> Seq.to_string
      )
end
