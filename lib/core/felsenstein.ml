open Core_kernel.Std
open Sigs
open TopoTree
open LATools

module Make (E:EVOL_MODEL) =
struct
  include E

  (* ======================= *)
  (* | Generic Felsenstein | *)
  (* ======================= *)
  let felsenstein_single ?(shift=fun _ _ v->v,0.0) param =
    let spec_eMt = eMt_mat param in
    fun ~site tree seq ->

      (* TEMPORARY: create zipper to use internally;
         to be replaced by zipper as input*)
      let zipper = Zipper.dzipper_of_tree tree in

      let rec aux z = match Zipper.location z.Zipper.zipper with
        | Zipper.LocLeaf -> Zipper.get_index z.Zipper.zipper |> leaf
        | _ -> let f1,f2,l,r =
                 Zipper.length_left z,
                 Zipper.length_right z,
                 Zipper.move_left z,
                 Zipper.move_right z
          in node f1 l f2 r

      and leaf i = Align.get_base seq ~seq:i ~pos:site
                   |> known_vector |> shift 0.0 0.0

      and node f1 l f2 r = match aux l, aux r with (v_l, s_l), (v_r, s_r) ->
        vec_vec_mul
          (mat_vec_mul (spec_eMt f1) v_l)
          (mat_vec_mul (spec_eMt f2) v_r)
        |> shift s_l s_r

      in let res_vec, res_shift = aux zipper in
      res_vec |> vec_vec_mul (stat_dist_vec param) |> sum_vec_elements |> log |> (+.) res_shift


  (* ============================ *)
  (* | Specific implementations | *)
  (* ============================ *)
  let shift_normal thre acc1 acc2 v =
    if min_vec v > thre then (v, acc1 +. acc2)
    else
      let mv = max_vec v in
      (scal_vec_mul v (1.0 /. mv), acc1 +. acc2 +. (log mv))

  let felsenstein_single_shift ?threshold:(threshold=0.0000001) param =
    felsenstein_single
      ~shift:(shift_normal threshold)
      param


  (* ======================= *)
  (* | Multi-site versions | *)
  (* ======================= *)
  let multisite (f: site:int -> TopoTree.t -> Align.t -> float) tree seq =
    let l = Align.length seq in
    List.fold (List.range 0 l) ~init:0.0 ~f:(fun acc x -> (f ~site:x tree seq) +. acc)

  let felsenstein_noshift param = multisite (felsenstein_single param)
  let felsenstein param = multisite (felsenstein_single_shift param)

end
