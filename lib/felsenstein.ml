open Core_kernel
open Phylogenetic_tree

module type Alignment = sig
  type t
  type base
  val get_base : t -> seq:string -> pos:int -> base
  val length : t -> int
end

module Make
    (A : Alphabet.S)
    (Align : Alignment with type base := A.t)
    (E : Site_evolution_model.S with type mat := A.matrix
                                 and type vec := A.vector) =
struct
  let known_vector x = A.Vector.init (fun y -> if A.equal x y then 1. else 0.)

  (* ======================= *)
  (* | Generic Felsenstein | *)
  (* ======================= *)
  let felsenstein_single ?(shift=fun _ _ v->v,0.0) param =
    (* First, specialize the eMt function to compute the diagonalization
       of the transition matrix once and for all. *)
    let spec_eMt = E.transition_probability_matrix param in

    fun ~site tree seq ->
      let rec aux = function (* then, go recursively through the topology tree *)
        | Leaf {index; _} -> leaf index
        | Node {left=l1,t1; right=l2,t2; _} -> node l1 t1 l2 t2

      (* On leaves, the probability vector is of the form (0,0,..,0,1,0,...0)
         where the position of the 1 is the position of the observed base. *)
      and leaf i = Align.get_base seq ~seq:i ~pos:site
                   |> known_vector |> shift 0.0 0.0
      (* there is no need to shift here, but the function is called anyways in case
         shift does not return the vector directly*)

      and node l1 t1 l2 t2 =
        let (v_l, s_l), (v_r, s_r) = aux t1, aux t2 in (* recursive calls *)
        A.Vector.mul
          (A.Matrix.apply (spec_eMt l1) v_l) (* vector of child x exp(branch_length x transition matrix) *)
          (A.Matrix.apply (spec_eMt l2) v_r)
        |> shift s_l s_r (* shift is a function that is responsible for preventing float underflows,
                            it is allowed to divide the result and carry the log along the tree*)

      in let res_vec, res_shift = aux tree in
      res_vec |> A.Vector.mul (E.stationary_distribution param) |> A.Vector.sum |> log |> (+.) res_shift
        (* In the end, multiply result vector by the static distribution of the model,
        then sum elements to get the likelihood, then take its log and add the log shifts
        that were performed to avoid underflows. *)


  (* ============================ *)
  (* | Specific implementations | *)
  (* ============================ *)
  let shift_normal thre acc1 acc2 v =
    (* if the min value of the vector is below a certain threshold... *)
    if Float.(A.Vector.min v > thre) then (v, acc1 +. acc2)
    else
      let mv = A.Vector.max v in (* then divide by its max and add the log of the max to *)
      (A.Vector.scal_mul (1.0 /. mv) v, acc1 +. acc2 +. (log mv)) (* an accumulator *)

  let felsenstein_single_shift ?threshold:(threshold=0.0000001) param =
    felsenstein_single
      ~shift:(shift_normal threshold)
      param


  (* ======================= *)
  (* | Multi-site versions | *)
  (* ======================= *)
  let multisite (f: site:int -> Phylogenetic_tree.t -> Align.t -> float) tree seq =
    let l = Align.length seq in
    List.fold (List.range 0 l) ~init:0.0 ~f:(fun acc x -> (f ~site:x tree seq) +. acc)

  let felsenstein_noshift param = multisite (felsenstein_single param)
  let felsenstein param = multisite (felsenstein_single_shift param)

end
