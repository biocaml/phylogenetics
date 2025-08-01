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
  type t = shifted_vector

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

  let scal_vec_mul alpha (SV (v, carry)) =
    SV (Vector.scal_mul alpha v, carry)

  let add (SV (v1, carry1)) (SV (v2, carry2)) =
    let v1, carry1, v2, carry2 =
      if Float.(carry1 > carry2) then v1, carry1, v2, carry2
      else v2, carry2, v1, carry1
    in
    let v2' = Vector.scal_mul (exp (carry2 -. carry1)) v2 in
    SV (Vector.add v1 v2', carry1)

  let%expect_test "SV add" =
    let x = 1.92403 and y = 6.35782 in
    let carry_x = 2. and carry_y = -1. in
    let SV (r, carry_r) =
      add
        (SV (Vector.of_array [|x|], carry_x))
        (SV (Vector.of_array [|y|], carry_y))
    in
    let r = Vector.get r 0 in
    printf "Expected: %f, got %f x exp %f = %f" (x *. exp carry_x +. y *. exp carry_y) r carry_r (r *. exp carry_r) ;
    [%expect {| Expected: 16.555677, got 2.240567 x exp 2.000000 = 16.555677 |}]

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

module Missing_values = struct
  let maybe_sv_mul x y = match x, y with
    | None, None -> None
    | Some cl, None
    | None, Some cl -> Some cl
    | Some cl1, Some cl2 -> Some (SV.mul cl1 cl2)

  let pruning t ~nstates ~transition_probabilities ~leaf_state ~root_frequencies =
    let open Let_syntax.Option in
    let rec tree (t : _ Tree.t) =
      match t with
      | Leaf l ->
        let+ state = leaf_state l in
        indicator ~i:state ~n:nstates
        |> SV.of_vec
      | Node n ->
        List1.fold n.branches ~init:None ~f:(fun acc b ->
            maybe_sv_mul acc (branch b)
          )
    and branch (Branch b) =
      let+ tip_value = tree b.tip in
      SV.decomp_vec_mul (transition_probabilities b.data) tip_value
    in
    match tree t with
    | Some res_tree ->
      let SV (v, carry) = SV.mul res_tree (SV.of_vec root_frequencies) in
      Float.log (Vector.sum v) +. carry
    | None -> 0.

  let conditional_likelihoods t ~nstates ~leaf_state ~transition_probabilities =
    let rec tree (t : _ Tree.t) =
      match t with
      | Leaf l -> (
          match leaf_state l with
          | None -> Tree.leaf None, None
          | Some state ->
            let cl = indicator ~i:state ~n:nstates in
            Tree.leaf (Some state), Some (SV.of_vec cl)
        )
      | Node n ->
        let branches, cls =
          List1.map n.branches ~f:branch
          |> List1.unzip
        in
        let maybe_cl = List1.reduce cls ~f:maybe_sv_mul in
        Tree.node maybe_cl branches, maybe_cl
    and branch ((Branch b) : _ Tree.branch) =
      let mat = transition_probabilities b.data in
      let tip, maybe_tip_cl = tree b.tip in
      let maybe_cl = Option.map maybe_tip_cl ~f:(fun tip_cl -> SV.mat_vec_mul mat tip_cl) in
      Tree.branch (b.data, mat) tip, maybe_cl
    in
    fst (tree t)

  let conditional_simulation rng t ~root_frequencies =
    let nstates = Vector.length root_frequencies in

    let dist_of_prior prior =
      Array.init nstates ~f:prior
      |> Gsl.Randist.discrete_preproc
    in
    let rec tree (t : _ Tree.t) prior =
      match t with
      | Leaf None ->
        let state = Gsl.Randist.discrete rng (dist_of_prior prior) in
        Tree.leaf state
      | Leaf (Some i) -> Tree.leaf i
      | Node n ->
        let weights = match n.data with
          | None -> dist_of_prior prior
          | Some (SV (conditional_likelihood, _)) ->
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
          set := Set.add !set i ;
          1.
        )
        else 0.
      )
    in
    if not (Set.is_empty !set) then Some (v, !set)
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
        Tree.leaf (Set.to_array set), SV.of_vec vec
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
          | 1 -> xs.(0)
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
        Hashtbl.find_or_add cache n ~default:(fun () ->
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

let conditional_simulation_along_branch_by_uniformization ~max_path_length { Uniformized_process._Q_ ; _P_ ; _R_ ; mu ; branch_length = lambda } ~rng ~start_state ~end_state =
  let nstates = fst (Linear_algebra.Matrix.dim _Q_) in
  let p_b_given_a_lambda = Matrix.get _P_ start_state end_state in
  let p_n_given_lambda n = Gsl.Randist.poisson_pdf n ~mu:(mu *. lambda) in
  let p_b_given_n_a n =
    if n = 0 then
      if start_state = end_state then 1. else 0.
    else
      Matrix.get (_R_ n) start_state end_state
  in
  let safe_sample_n_given_a_b_lambda () =
    Array.init (max_path_length + 1) ~f:(fun i ->
        p_n_given_lambda i *. p_b_given_n_a i
      )
    |> Gsl.Randist.discrete_preproc
    |> Gsl.Randist.discrete rng
  in
  let sample_n_given_a_b_lambda () =
    let g = p_b_given_a_lambda *. Gsl.Rng.uniform rng in
    let rec loop acc i =
      if i > max_path_length then safe_sample_n_given_a_b_lambda ()
      else
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

type rejection_sampling_error = {
  max_tries : int ;
  start_state : int ;
  end_state : int ;
  rate : float ;
  total_rate : float ;
  prob : float ;
}

let conditional_simulation_along_branch_by_rejection_sampling ~rate_matrix ~max_tries ~rng ~branch_length ~start_state ~end_state ~init ~f =
  let nstates = fst (Linear_algebra.Matrix.dim rate_matrix) in
  let module A = Alphabet.Make(struct let card = nstates end) in
  let module BI = struct type t = float let length = Fn.id end in
  let module Sim = Simulator.Make(A)(BI) in
  let rec loop remaining_tries =
    if remaining_tries <= 0 then
      let rate = Linear_algebra.Matrix.get rate_matrix start_state end_state in
      let total_rate = sum nstates (fun dest ->
          if dest = start_state then 0. else Linear_algebra.Matrix.get rate_matrix start_state dest
        )
      in
      let prob = rate /. total_rate in
      Error { max_tries ;
              start_state ; end_state ; rate ; total_rate ; prob }
    else
      let res, simulated_end_state =
        Sim.branch_gillespie_direct rng
          ~start_state ~rate_matrix ~branch_length
          ~init:(init, start_state) ~f:(fun (acc, _) s t -> f acc s t, s)
      in
      if A.equal simulated_end_state end_state then Ok res
      else loop (remaining_tries - 1)
  in
  loop max_tries

let conditional_simulation_along_branch_by_rejection_sampling ~rate_matrix ~max_tries ~rng ~branch_length ~start_state ~end_state =
  conditional_simulation_along_branch_by_rejection_sampling
    ~rate_matrix ~max_tries ~rng ~branch_length ~start_state ~end_state
    ~init:[] ~f:(fun acc s t -> (s, t) :: acc)
  |> Result.map ~f:Array.of_list_rev

module Path_sampler = struct
  type t =
    | Uniformization_sampler of { up : Uniformized_process.t ; max_path_length : int }
    | Rejection_sampler of { rates : mat ; max_tries : int ; branch_length : float }
    | Rejection_sampler_or_uniformization of {
        max_tries : int ;
        up : Uniformized_process.t ;
        max_path_length : int ;
      }

  let uniformization ~max_path_length up =
    Uniformization_sampler { max_path_length ; up }

  let rejection_sampling ~max_tries ~rates ~branch_length () =
    Rejection_sampler { rates ; max_tries ; branch_length }

  let rejection_sampling_or_uniformization ~max_tries ~max_path_length up =
    Rejection_sampler_or_uniformization { max_tries ; max_path_length ; up }

  let sample_exn meth ~rng ~start_state ~end_state =
    match meth with
    | Uniformization_sampler { max_path_length ; up } ->
      conditional_simulation_along_branch_by_uniformization ~max_path_length up ~rng ~start_state ~end_state
    | Rejection_sampler { rates = rate_matrix ; max_tries ; branch_length } -> (
        match
          conditional_simulation_along_branch_by_rejection_sampling ~rate_matrix ~max_tries ~branch_length ~rng ~start_state ~end_state
        with
        | Ok res -> res
        | Error { max_tries ; start_state ; end_state ; rate ; total_rate ; prob } ->
          failwithf
            "Reached max (= %d) tries: %d -[%f]-> %d (rate = %g, total_rate = %g, prob = %g)"
            max_tries
            start_state branch_length end_state rate total_rate prob ()
      )
    | Rejection_sampler_or_uniformization { max_tries ; up ; max_path_length } ->
      match
        conditional_simulation_along_branch_by_rejection_sampling
          ~rng ~start_state ~end_state
          ~rate_matrix:(Uniformized_process.transition_rates up)
          ~max_tries
          ~branch_length:up.branch_length
      with
      | Ok res -> res
      | Error _ -> conditional_simulation_along_branch_by_uniformization up ~max_path_length ~rng ~start_state ~end_state

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
