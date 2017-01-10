open Core_kernel.Std
open Sigs
open TopoTree
open LATools

module Make (E:EVOL_MODEL) =
struct
  include Model_utils.Make (E)

  (* ======================= *)
  (* | Generic Felsenstein | *)
  (* ======================= *)
  let felsenstein_single
      ?(shift=fun _ _ v->v,0.0)
      ~site
      () param tree seq
    =
    let rec aux = function
      | Node ((f1,l), (f2,r)) -> node f1 l f2 r
      | Leaf i -> leaf i

    and leaf i = Align.get_base seq ~seq:i ~pos:site
                 |> known_vector |> shift 0.0 0.0

    and node f1 l f2 r = match aux l, aux r with (v_l, s_l), (v_r, s_r) ->
      vec_vec_mul
        (mat_vec_mul (eMt param f1) v_l)
        (mat_vec_mul (eMt param f2) v_r)
      |> shift s_l s_r

    in let res_vec, res_shift = aux tree in
    res_vec |> vec_vec_mul (stat_dist_vec param) |> sum_vec_elements |> log |> (+.) res_shift


  (* ============================ *)
  (* | Specific implementations | *)
  (* ============================ *)
  let shift_normal thre acc1 acc2 v =
    if min_vec v > thre then (v, acc1 +. acc2)
    else
      let mv = max_vec v in
      (scal_vec_mul v (1.0 /. mv), acc1 +. acc2 +. (log mv))

  let felsenstein_single_shift ?threshold:(threshold=0.0000001) ~site () =
    felsenstein_single
      ~shift:(shift_normal threshold)
      ~site:site ()


  (* ======================= *)
  (* | Multi-site versions | *)
  (* ======================= *)
  let multisite (f: site:int -> E.t -> TopoTree.t -> Align.t -> float) param tree seq =
    let l = Align.length seq in
    List.fold (List.range 0 l) ~init:0.0 ~f:(fun acc x -> (f ~site:x param tree seq) +. acc)

  let felsenstein_noshift () = multisite (felsenstein_single ())
  let felsenstein () = multisite (felsenstein_single_shift ())

end
