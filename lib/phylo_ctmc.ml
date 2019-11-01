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
    val init : int -> f:(int -> float) -> vec
    val mul : t -> t -> t
    val min : t -> float
    val max : t -> float
    val sum : t -> float
  end
  module Mat : sig
    type t = mat
    val row : t -> int -> vec
  end
  val scal_vec_mul : float -> vec -> vec
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

    let mul (SV (v1, carry1)) (SV (v2, carry2)) =
      Vec.mul v1 v2
      |> shift ~carry:(carry1 +. carry2)
  end

  let indicator ~i ~n = L.Vec.init n ~f:(fun j -> if i = j then 1. else 0.)

  let pruning t ~transition_matrix ~leaf_state ~root_frequencies =
    let rec tree (t : _ Tree.t) =
      match t with
      | Leaf l ->
        let state = leaf_state l in
        indicator ~i:(A.to_int state) ~n:A.card
        |> SV.of_vec
      | Node n ->
        Non_empty_list.map n.branches ~f:(fun (Branch b) ->
            SV.mat_vec_mul (transition_matrix b.data) (tree b.tip)
          )
        |> Non_empty_list.to_list
        |> List.reduce_exn ~f:SV.mul
    in
    let SV (v, carry) = SV.mul (tree t) (SV.of_vec root_frequencies) in
    Float.log (Vec.sum v) +. carry

  let conditionial_likelihoods t ~transition_matrix ~leaf_state =
    let rec tree (t : _ Tree.t) =
      match t with
      | Leaf l ->
        let state = leaf_state l in
        let cl = indicator ~i:(A.to_int state) ~n:A.card in
        Tree.leaf (SV.of_vec cl)

      | Node n ->
        let children = Non_empty_list.map n.branches ~f:branch in
        let cl =
          Non_empty_list.map children ~f:Tree.(fun (Branch b) ->
            SV.mat_vec_mul b.data (Tree.data b.tip)
            )
          |> Non_empty_list.to_list
          |> List.reduce_exn ~f:SV.mul
        in
        Tree.Node { data = cl ; branches = children }
    and branch ((Branch b) : _ Tree.branch) =
      let mat = transition_matrix b.data in
      Tree.branch mat (tree b.tip)
    in
    tree t

  let conditional_simulation t ~root_frequencies ~choose =
    let rec tree (t : _ Tree.t) prior =
      let SV (conditional_likelihood, _) = Tree.data t in
      let state =
        Vec.mul prior conditional_likelihood
        |> choose
      in
      match t with
      | Leaf _ -> Tree.leaf state
      | Node n ->
        let branches = Non_empty_list.map n.branches ~f:(fun br -> branch br state) in
        Tree.node state branches

    and branch (Branch br) parent_state =
      let prior = Mat.row br.data parent_state in
      (* FIXME: lacaml is not good at getting a row (while it is for
         columns). Maybe change operations so that this operation is
         avoided? *)
      Tree.branch br.data (tree br.tip prior)
    in
    tree t root_frequencies
end
