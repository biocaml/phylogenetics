open Core
open Linear_algebra

type matrix_decomposition = [
  | `Mat of mat
  | `Transpose of mat
  | `Diag of vec
] list

let identity dim =
  Matrix.init dim ~f:(fun i j -> if i = j then 1. else 0.)

let rec matrix_decomposition_reduce ~dim = function
  | [] -> identity dim
  | [ `Mat m ] -> m
  | [ `Diag v ] -> Matrix.diagm v
  | [ `Transpose m ] -> Matrix.transpose m
  | h :: t ->
    let m_t = matrix_decomposition_reduce ~dim t in
    match h with
    | `Mat m_h -> Matrix.dot m_h m_t
    | `Transpose m_h -> Matrix.dot ~transa:`T m_h m_t
    | `Diag v -> Matrix.dot (Matrix.diagm v) m_t (* FIXME: don't build the diagonal matrix *)
(* FIXME: other cases could be optimized, like [ _ ; `Transpose m] or [_ ; `Diag v ] *)

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

  let decomp_vec_mul_aux op v = match op with
    | `Mat m -> Matrix.apply m v
    | `Transpose m -> Matrix.apply ~trans:`T m v
    | `Diag d -> Vector.mul d v

  let decomp_vec_mul decomp (SV (v, carry)) =
    SV (List.fold_right decomp ~init:v ~f:decomp_vec_mul_aux, carry)

  let mat_vec_mul mat (SV (v, carry)) =
    SV (Matrix.apply mat v, carry)

  let mul (SV (v1, carry1)) (SV (v2, carry2)) =
    Vector.mul v1 v2
    |> shift ~carry:(carry1 +. carry2)
end

let indicator ~i ~n = Vector.init n ~f:(fun j -> if i = j then 1. else 0.)

let pruning t ~nstates ~transition_probabilities ~leaf_state ~root_frequencies =
  let rec tree (t : _ Tree.t) =
    match t with
    | Leaf l ->
      let state = leaf_state l in
      indicator ~i:state ~n:nstates
      |> SV.of_vec
    | Node n ->
      List1.map n.branches ~f:(fun (Branch b) ->
          SV.decomp_vec_mul (transition_probabilities b.data) (tree b.tip)
        )
      |> List1.to_list
      |> List.reduce_exn ~f:SV.mul
  in
  let SV (v, carry) = SV.mul (tree t) (SV.of_vec root_frequencies) in
  Float.log (Vector.sum v) +. carry

let pruning_with_missing_values t ~nstates ~transition_probabilities ~leaf_state ~root_frequencies =
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
          SV.decomp_vec_mul (transition_probabilities b.data) tip_term
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

let conditional_likelihoods t ~nstates ~leaf_state ~transition_probabilities =
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
    let mat = transition_probabilities b.data in
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

module Ambiguous = struct

  let leaf_indicator ~nstates state =
    let at_least_one = ref false in
    let v = Vector.init nstates ~f:(fun i ->
        if state i then (at_least_one := true ; 1.)
        else 0.
      )
    in
    if !at_least_one then Some v
    else None

  let leaf_indicator_with_set ~nstates state =
    let set = ref Int.Set.empty in
    let v = Vector.init nstates ~f:(fun i ->
        if state i then (
          set := Int.Set.add !set i ;
          1.
        )
        else 0.
      )
    in
    if not (Int.Set.is_empty !set) then Some (v, !set)
    else None

  (* Pruning for a tree (1 (2 3)) computes

       \pi (P_2 I_{x_2} \otimes P_3 I_{x_3}) (1)

     where \pi is the vector of root frequecies, P_2 (resp P_3) the
     transition probability matrix from 1 to 2 (resp to 3), I_x is the
     indicator vector corresponding to state x, x_2 (resp x_3) is the
     observed state in leaf 2 (resp leaf 3), and \otimes is the
     Kronecker product.

     Now if we only observed state in leaf 2 is inside a set A, we ought
     to compute the likelihood

     Prob(X_2 \in A, X_3 = x_3) = \sum_{x_2 \in A} Prob(X_2 = x_2, X_3 =
     x_3)

     (1) becomes

     \pi (P_2 (\sum_{x_2 \in A} I_{x_2}) \otimes P_3 I_{x_3})

     by linearity of matrix-vector multiplication and \otimes *)
  let pruning t ~nstates ~transition_probabilities ~leaf_state ~root_frequencies =
    let open Option.Let_syntax in
    let rec tree (t : _ Tree.t) =
      match t with
      | Leaf l ->
        let state = leaf_state l in
        let%map vec = leaf_indicator ~nstates state in
        SV.of_vec vec
      | Node n ->
        let terms = List1.filter_map n.branches ~f:(fun (Branch b) ->
            let%map tip_term = tree b.tip in
            SV.decomp_vec_mul (transition_probabilities b.data) tip_term
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

  let conditional_likelihoods t ~nstates ~leaf_state ~transition_probabilities =
    let open Option.Let_syntax in
    let rec node (t : _ Tree.t) =
      match t with
      | Leaf l ->
        let state = leaf_state l in
        let%map vec, set = leaf_indicator_with_set ~nstates state in
        Tree.leaf (Int.Set.to_array set), SV.of_vec vec
      | Node n ->
        match List1.filter_map n.branches ~f:branch with
        | [] -> None
        | (b0, cl0) :: rest ->
          let branches, cl =
            List.fold rest ~init:(List1.singleton b0, cl0) ~f:(fun (branches, cl) (b_i, cl_i) ->
                List1.cons1 b_i branches,
                SV.mul cl cl_i
              )
          in
          Some (Tree.node cl (List1.rev branches), cl)
    and branch ((Branch b) : _ Tree.branch) =
      let mat = transition_probabilities b.data in
      let%map tip, tip_cl = node b.tip in
      let cl = SV.mat_vec_mul mat tip_cl in
      Tree.branch (b.data, mat) tip, cl
    in
    match node t with
    | Some (t', _) -> t'
    | None ->
      Leaf (Array.init nstates ~f:Fun.id)

  let conditional_simulation rng t ~root_frequencies =
    let nstates = Vector.length root_frequencies in
    let rec tree (t : _ Tree.t) prior =
      match t with
      | Leaf xs ->
        let state = match Array.length xs with
          | 0 -> failwith "invalid conditional likelihood tree"
          | 1 -> xs.(1)
          | _ ->
            let weights =
              Array.map xs ~f:prior
              |> Gsl.Randist.discrete_preproc
            in
            xs.(Gsl.Randist.discrete rng weights)
        in
        Tree.leaf state
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
end

module Uniformized_process = struct
  type t = {
    _Q_ : mat ; (* transition rates *)
    _P_ : mat ; (* transition_probabilities *)
    _R_ : int -> mat ; (* transition probabilities in uniformized process, R^n *)
    mu : float ; (* uniformized rate \mu *)
    branch_length : float ;
  }

  let range_map_reduce a b ~map ~reduce =
    if b <= a then invalid_arg "empty range" ;
    let rec loop i acc =
      if i = b then acc
      else loop (i + 1) (reduce acc (map i))
    in
    loop (a + 1) (map a)

  let make ~transition_rates:_Q_ ~transition_probabilities =
    let dim =
      let m, n = Matrix.dim _Q_ in
      if m <> n then invalid_arg "Expected squared matrix" ;
      m
    in
    let mu = range_map_reduce 0 dim ~map:(fun i-> -. Matrix.get _Q_ i i) ~reduce:Float.max in
    if Float.(mu <= 0.) then invalid_arg "invalid rate matrix" ;
    let _R_ = Matrix.init dim ~f:(fun i j -> Matrix.get _Q_ i j /. mu +. if i = j then 1. else 0.) in
    let cache = Int.Table.create () in
    let rec pow_R n =
      assert (n >= 0) ;
      if n = 0 then Matrix.init dim ~f:(fun i j -> if i = j then 1. else 0.)
      else if n = 1 then _R_
      else
        Int.Table.find_or_add cache n ~default:(fun () ->
            Matrix.dot (pow_R (n - 1)) _R_
          )
    in
    Staged.stage (
      fun ~branch_length ->
        let _P_ = transition_probabilities branch_length in
        { _Q_ ; _P_ ; _R_ = pow_R ; mu ; branch_length }
    )

  let transition_rates up = up._Q_
  let transition_probabilities up = up._P_
end

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

let conditional_simulation_along_branch_by_uniformization { Uniformized_process._Q_ ; _P_ ; _R_ ; mu ; branch_length = lambda } ~rng ~start_state ~end_state =
  let nstates = fst (Linear_algebra.Matrix.dim _Q_) in
  let p_b_given_a_lambda = Matrix.get _P_ start_state end_state in
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

let sum n f =
  let rec loop acc i =
    if i = n then acc
    else loop (acc +. f i) (i + 1)
  in
  loop 0. 0

let conditional_simulation_along_branch_by_rejection_sampling ~rate_matrix ~max_tries ~rng ~branch_length ~start_state ~end_state ~init ~f =
  let nstates = fst (Linear_algebra.Matrix.dim rate_matrix) in
  let module A = Alphabet.Make(struct let card = nstates end) in
  let module BI = struct type t = float let length = Fn.id end in
  let module Sim = Simulator.Make(A)(BI) in
  let rec loop remaining_tries =
    if Option.value_map remaining_tries ~default:false ~f:(( = ) 0) then (
      let rate = Linear_algebra.Matrix.get rate_matrix start_state end_state in
      let total_rate = sum nstates (fun dest ->
          if dest = start_state then 0. else Linear_algebra.Matrix.get rate_matrix start_state dest
        )
      in
      let prob = rate /. total_rate in
      failwithf
        "Reached max (= %d) tries: %d -[%f]-> %d (rate = %g, total_rate = %g, prob = %g)"
        (match max_tries with None -> assert false | Some x -> x)
        start_state branch_length end_state rate total_rate prob ()
    )
    else
      let res, simulated_end_state =
        Sim.branch_gillespie_direct rng
          ~start_state ~rate_matrix ~branch_length
          ~init:(init, start_state) ~f:(fun (acc, _) s t -> f acc s t, s)
      in
      if A.equal simulated_end_state end_state then res
      else loop (Option.map remaining_tries ~f:pred)
  in
  loop max_tries

let conditional_simulation_along_branch_by_rejection_sampling ~rate_matrix ~max_tries ~rng ~branch_length ~start_state ~end_state =
  conditional_simulation_along_branch_by_rejection_sampling
    ~rate_matrix ~max_tries ~rng ~branch_length ~start_state ~end_state
    ~init:[] ~f:(fun acc s t -> (s, t) :: acc)
  |> Array.of_list_rev

module Path_sampler = struct
  type t =
    | Uniformization_sampler of Uniformized_process.t
    | Rejection_sampler of { rates : mat ; max_tries : int option ; branch_length : float }

  let uniformization process =
    Uniformization_sampler process

  let rejection_sampling ?max_tries ~rates ~branch_length () =
    Rejection_sampler { rates ; max_tries ; branch_length }

  let sample_exn meth =
    match meth with
    | Uniformization_sampler up ->
      conditional_simulation_along_branch_by_uniformization up
    | Rejection_sampler { rates = rate_matrix ; max_tries ; branch_length } ->
      conditional_simulation_along_branch_by_rejection_sampling ~rate_matrix ~max_tries ~branch_length
end

let substitution_mapping ~rng ~path_sampler sim =
  let rec traverse_node = function
    | Tree.Leaf l -> Tree.leaf l
    | Node n ->
      Tree.node n.data (List1.map n.branches ~f:(traverse_branch n.data))
  and traverse_branch a (Tree.Branch br) =
    let b = Tree.data br.tip in
    let branch_data = fst br.data in
    let data =
      Path_sampler.sample_exn (path_sampler branch_data)
        ~rng ~start_state:a ~end_state:b in
    Tree.branch (branch_data, data) (traverse_node br.tip)
  in
  traverse_node sim
