open Core
open Linear_algebra

type shifted_vector = SV of vec * float
module SV = struct
  let of_vec v = SV (v, 0.)

  let shift ?(threshold = 1e-6) v ~carry =
    if Float.(Vector.min v > threshold) then (SV (v, carry))
    else
      let mv = Vector.max v in
      SV (
        Vector.scal_mul (1. /. mv) v,
        carry +. log mv
      )

  let mat_vec_mul mat (SV (v, carry)) =
    SV (Matrix.apply mat v, carry)

  let mul (SV (v1, carry1)) (SV (v2, carry2)) =
    Vector.mul v1 v2
    |> shift ~carry:(carry1 +. carry2)
end

let indicator ~i ~n = Vector.init n ~f:(fun j -> if i = j then 1. else 0.)

let pruning t ~nstates ~transition_matrix ~leaf_state ~root_frequencies =
  let rec tree (t : _ Tree.t) =
    match t with
    | Leaf l ->
      let state = leaf_state l in
      indicator ~i:state ~n:nstates
      |> SV.of_vec
    | Node n ->
      List1.map n.branches ~f:(fun (Branch b) ->
          SV.mat_vec_mul (transition_matrix b.data) (tree b.tip)
        )
      |> List1.to_list
      |> List.reduce_exn ~f:SV.mul
  in
  let SV (v, carry) = SV.mul (tree t) (SV.of_vec root_frequencies) in
  Float.log (Vector.sum v) +. carry

let pruning_with_missing_values t ~nstates ~transition_matrix ~leaf_state ~root_frequencies =
  let open Option.Let_syntax in
  let rec tree (t : _ Tree.t) =
    match t with
    | Leaf l ->
      let%map state = leaf_state l in
      indicator ~i:state ~n:nstates
      |> SV.of_vec
    | Node n ->
      let terms = List1.filter_map n.branches ~f:(fun (Branch b) ->
          let%map tip_term = tree b.tip in
          SV.mat_vec_mul (transition_matrix b.data) tip_term
        )
      in
      match terms with
      | [] -> None
      | _ :: _ as xs ->
        List.reduce_exn xs ~f:SV.mul
        |> Option.some
  in
  match tree t with
  | Some res_tree ->
    let SV (v, carry) = SV.mul res_tree (SV.of_vec root_frequencies) in
    Float.log (Vector.sum v) +. carry
  | None -> 0.

let normalize_opt vec =
  let total = Vector.sum vec in
  if Float.(total = 0.)
  then None
  else Some (Vector.map vec ~f:(fun x -> x /. total))

let leaf_probs ~nstates state =
  Vector.init nstates ~f:(fun i -> if state i then 1. else 0.)
  |> normalize_opt

let pruning_with_multiple_states t ~nstates ~transition_matrix ~leaf_state ~root_frequencies =
  let open Option.Let_syntax in
  let rec tree (t : _ Tree.t) =
    match t with
    | Leaf l ->
      let state = leaf_state l in
      let%map vec = leaf_probs ~nstates state in
      SV.of_vec vec
    | Node n ->
      let terms = List1.filter_map n.branches ~f:(fun (Branch b) ->
          let%map tip_term = tree b.tip in
          SV.mat_vec_mul (transition_matrix b.data) tip_term
        )
      in
      match terms with
      | [] -> None
      | _ :: _ as xs ->
        List.reduce_exn xs ~f:SV.mul
        |> Option.some
  in
  match tree t with
  | Some res_tree ->
    let SV (v, carry) = SV.mul res_tree (SV.of_vec root_frequencies) in
    Float.log (Vector.sum v) +. carry
  | None -> 0.

let conditionial_likelihoods t ~nstates ~leaf_state ~transition_matrix =
  let rec tree (t : _ Tree.t) =
    match t with
    | Leaf l ->
      let state = leaf_state l in
      let cl = indicator ~i:state ~n:nstates in
      Tree.leaf state, SV.of_vec cl
    | Node n ->
      let children, cls =
        List1.map n.branches ~f:branch
        |> List1.unzip
      in
      let cl = List1.reduce cls ~f:SV.mul in
      Tree.node cl children, cl
  and branch ((Branch b) : _ Tree.branch) =
    let mat = transition_matrix b.data in
    let tip, tip_cl = tree b.tip in
    let cl = SV.mat_vec_mul mat tip_cl in
    Tree.branch (b.data, mat) tip, cl
  in
  fst (tree t)

let conditional_simulation rng t ~root_frequencies =
  let nstates = Vector.length root_frequencies in
  let rec tree (t : _ Tree.t) prior =
    match t with
    | Leaf i -> Tree.leaf i
    | Node n ->
      let SV (conditional_likelihood, _) = n.data in
      let weights =
        Array.init nstates ~f:(fun i ->
            prior i *. Vector.get conditional_likelihood i
          )
        |> Gsl.Randist.discrete_preproc
      in
      let state = Gsl.Randist.discrete rng weights in
      let branches = List1.map n.branches ~f:(fun br -> branch br state) in
      Tree.node state branches

  and branch (Branch br) parent_state =
    let prior i = Matrix.get (snd br.data) parent_state i in
    Tree.branch br.data (tree br.tip prior)
  in
  tree t (Vector.get root_frequencies)

type uniformized_process = {
  _Q_ : mat ; (* transition rates *)
  _P_ : float -> mat ; (* transition_probabilities, lambda -> exp (lambda Q) *)
  _R_ : int -> mat ; (* transition probabilities in uniformized process, R^n *)
  mu : float ; (* uniformized rate \mu *)
}

let range_map_reduce a b ~map ~reduce =
  if b <= a then invalid_arg "empty range" ;
  let rec loop i acc =
    if i = b then acc
    else loop (i + 1) (reduce acc (map i))
  in
  loop (a + 1) (map a)

let uniformized_process rates =
  let _Q_ = rates in
  let _P_ = fun lambda -> Matrix.(expm (scal_mul lambda _Q_)) in
  let m, n = Matrix.dim _Q_ in
  if m <> n then invalid_arg "square matrix expected" ;
  let mu = range_map_reduce 0 n ~map:(fun i-> -. Matrix.get _Q_ i i) ~reduce:Float.max in
  let _R_ = Matrix.init n ~f:(fun i j -> Matrix.get _Q_ i j /. mu +. if i = j then 1. else 0.) in
  let cache = Int.Table.create () in
  let rec pow_R n =
    assert (n >= 0) ;
    if n = 0 then Matrix.init (fst (Matrix.dim rates)) ~f:(fun i j -> if i = j then 1. else 0.)
    else if n = 1 then _R_
    else
      Int.Table.find_or_add cache n ~default:(fun () ->
          Matrix.dot (pow_R (n - 1)) _R_
        )
  in
  { _Q_ ; _P_ ; _R_ = pow_R ; mu }

let array_recurrent_init n ~init ~f =
  if n = 0 then [| |]
  else
    let r = Array.create ~len:n (f ~prec:init 0) in
    for i = 1 to n - 1 do
      r.(i) <- f ~prec:r.(i - 1) i
    done ;
    r

let collapse_mapping ~start_state path times =
  assert Array.(length path = length times) ;
  let n = Array.length path in
  let rec loop prec_state i acc =
    if i = n then acc
    else
      let acc' = if path.(i) <> prec_state then (path.(i), times.(i)) :: acc else acc in
      loop path.(i) (i + 1) acc'
  in
  loop start_state 0 []
  |> Array.of_list_rev

let conditional_simulation_along_branch rng { _Q_ ; _P_ ; _R_ ; mu } ~branch_length:lambda ~start_state ~end_state ~nstates =
  let p_b_given_a_lambda = Matrix.get (_P_ lambda) start_state end_state in
  let p_n_given_lambda n = Gsl.Randist.poisson_pdf n ~mu:(mu *. lambda) in
  let p_b_given_n_a n =
    if n = 0 then
      if start_state = end_state then 1. else 0.
    else
      Matrix.get (_R_ n) start_state end_state
  in
  let sample_n_given_a_b_lambda () =
    let g = p_b_given_a_lambda *. Gsl.Rng.uniform rng in
    let rec loop acc i =
      let acc' = acc +. p_n_given_lambda i *. p_b_given_n_a i in
      if Float.(acc' > g) then i
      else loop acc' (i + 1)
    in
    loop 0. 0
  in
  let n = sample_n_given_a_b_lambda () in
  let sample_path () =
    let _R_1 = _R_ 1 in
    array_recurrent_init n ~init:start_state ~f:(fun ~prec:current_state i ->
        let transition_probability =
          let _R_second_half = _R_ (n - i - 1) in
          fun s ->
            Matrix.get _R_1 current_state s
            *. Matrix.get _R_second_half s end_state
        in
        let weights =
          Array.init nstates ~f:transition_probability
          |> Gsl.Randist.discrete_preproc
        in
        Gsl.Randist.discrete rng weights
      )
  in
  let path = sample_path () in
  let times =
    let r = Array.init n  ~f:(fun _ -> Gsl.Rng.uniform rng *. lambda) in
    Array.sort r ~compare:Float.compare ;
    r
  in
  collapse_mapping path times ~start_state

let substitution_mapping ~nstates ~branch_length ~rng ~process sim =
  let rec traverse_node = function
    | Tree.Leaf l -> Tree.leaf l
    | Node n ->
      Tree.node n.data (List1.map n.branches ~f:(traverse_branch n.data))
  and traverse_branch a (Tree.Branch br) =
    let b = Tree.data br.tip in
    let branch_data = fst br.data in
    let branch_length = branch_length branch_data in
    let data = conditional_simulation_along_branch rng (process branch_data) ~nstates ~branch_length ~start_state:a ~end_state:b in
    Tree.branch (branch_data, data) (traverse_node br.tip)
  in
  traverse_node sim
