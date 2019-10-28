open Core_kernel

module type Alphabet = sig
  type t
  val card : int
  val to_int : t -> int
  type 'a vector
  val vector : (t -> 'a) -> 'a vector
end

module type Linalg = sig
  type vec
  type mat
  module Vec : sig
    type t = vec
    val get : t -> int -> float
    val indicator :
      i:int ->
      n:int ->
      vec
    val min : t -> float
    val max : t -> float
    val sum : t -> float
    val choose : t -> int
  end
  module Mat : sig
    type t = mat
    val row : t -> int -> vec
  end
  val scal_vec_mul : float -> vec -> vec
  val vec_vec_mul : vec -> vec -> vec
  val mat_vec_mul : mat -> vec -> vec
end

module Make(A : Alphabet)(L : Linalg) = struct
  open L

  type shifted_vector = SV of vec * float
  module SV = struct
    let of_vec v = SV (v, 0.)

    let shift ?(threshold = 1e-6) v ~carry =
      if Vec.min v > threshold then (SV (v, carry))
      else
        let mv = Vec.max v in
        SV (
          scal_vec_mul (1. /. mv) v,
          carry +. log mv
        )

    let mat_vec_mul mat (SV (v, carry)) =
      SV (mat_vec_mul mat v, carry)

    let vec_vec_mul (SV (v1, carry1)) (SV (v2, carry2)) =
      vec_vec_mul v1 v2
      |> shift ~carry:(carry1 +. carry2)
  end

  let pruning tree ~transition_matrix ~leaf_state ~root_frequencies =
    let rec node (n : _ Tree.t) =
      match n.branches with
      | [] ->
        let state = leaf_state n.node_data in
        Vec.indicator ~i:(A.to_int state) ~n:A.card
        |> SV.of_vec
      | _ :: _ ->
        List.map n.branches ~f:(fun b ->
            SV.mat_vec_mul (transition_matrix b) (node b.tip)
          )
        |> List.reduce_exn ~f:SV.vec_vec_mul
    in
    let SV (v, carry) = SV.vec_vec_mul (node tree) (SV.of_vec root_frequencies) in
    Float.log (Vec.sum v) +. carry
    

  let conditionial_likelihoods tree ~transition_matrix ~leaf_state =
    let rec node (n : _ Tree.t) =
      match n.branches with
      | [] ->
        let state = leaf_state n.node_data in
        let cl = Vec.indicator ~i:(A.to_int state) ~n:A.card in
        Tree.node (SV.of_vec cl) []
        
      | _ :: _ ->
        let children = List.map n.branches ~f:branch in
        let cl =
          List.map children ~f:Tree.(fun b ->
            SV.mat_vec_mul b.branch_data b.tip.node_data
          )
          |> List.reduce_exn ~f:SV.vec_vec_mul
        in
        Tree.node cl children
    and branch (b : _ Tree.branch) =
      let mat = transition_matrix b in
      Tree.branch mat (node b.tip)
    in
    node tree

  let conditional_simulation tree ~root_frequencies =
    let rec node (n : _ Tree.t) prior =
      let SV (conditional_likelihood, _) = n.node_data in 
      let state = 
        vec_vec_mul prior conditional_likelihood
        |> Vec.choose
      in
      Tree.node
        state
        (List.map n.branches ~f:(fun br -> branch br state))
    and branch br parent_state =
      let prior = Mat.row br.branch_data parent_state in
      Tree.branch br.branch_data (node br.tip prior)
    in
    node tree root_frequencies
end
