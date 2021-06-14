open Core_kernel
open Phylogenetics
open Phylogenetics.Linear_algebra.Lacaml

module AAGTR = Site_evolution_model.Amino_acid_GTR

let aa_sum ?except f =
  let f =
    match except with
    | None -> fun acc i -> acc +. f i
    | Some aa -> fun acc i -> if Amino_acid.equal aa i then acc else acc +. f i
  in
  List.fold Amino_acid.all ~init:0. ~f

let time ~f x =
  let t = Core.Unix.gettimeofday () in
  let fx = f x in
  let runtime = Core.Unix.gettimeofday () -. t in
  runtime, fx

let rng = Gsl.Rng.(make (default ()))

let () = Gsl.Rng.set rng (Nativeint.of_int 3452345)

let nstates = Amino_acid.card

let nelder_mead_init theta0 =
  let c = ref (-1) in
  fun _ ->
    incr c ;
    if !c = 0 then theta0
    else
      Array.init (Array.length theta0) ~f:(fun i ->
          theta0.(i) +. if i = !c - 1 then  -. 1. else 0.
        )

let sample_tree ntaxa =
  let bdp = Birth_death.make ~birth_rate:2. ~death_rate:1. in
  Birth_death.age_ntaxa_simulation bdp rng ~age:1. ~ntaxa
  (* Tree.node  () (List1.singleton (Tree.branch 0.1 @@ Tree.leaf 0)) *)

let sample_profile () =
  let disp = 1. in
  Amino_acid.Vector.init (fun _ ->
      Gsl.Randist.gamma rng ~a:(1. /. disp) ~b:(1. /. disp)
    )
  |> Amino_acid.Vector.normalize

let sample_param wag =
  { AAGTR.exchangeability_matrix = wag.Wag.rate_matrix ;
    scale = Gsl.Rng.uniform rng +. 0.5 ;
    stationary_distribution = sample_profile () }

module Branch_info = struct
  type t = float
  let length x = x
end

module AASim = Phylogenetics.Simulator.Make(Amino_acid)(Branch_info)

let valine = Option.value_exn (Amino_acid.of_char 'V')
let alanine = Option.value_exn (Amino_acid.of_char 'A')

let wag = Wag.parse "../data/wag.dat"

let wag_param = {
  AAGTR.stationary_distribution = wag.freqs ;
  exchangeability_matrix = wag.rate_matrix ;
  scale = 1. ;
}

(*
  Test by comparing histogram of events along a branch using rejection
  sampling or uniformized sampling
*)

let int_histogram xs =
  let n = List.length xs in
  Biocaml_unix.Accu.counts (Stream.of_list xs)
  |> CFStream.Stream.to_list
  |> List.map ~f:(fun (i, k) -> i, float k /. float n)
  |> List.sort ~compare:Poly.compare

let render_int_histogram xs =
  int_histogram xs
  |> List.map ~f:(fun (k, f) -> sprintf "%02d %.3f" k f)
  |> String.concat ~sep:" | "
  |> print_endline

let simulation_along_branch_by_rejection_sampling rng ~param ~branch_length ~start_state ~end_state ~init ~f =
  let rate_matrix = AAGTR.rate_matrix param in
  let rec loop () =
    let res, simulated_end_state =
      AASim.branch_gillespie_direct rng
        ~start_state ~rate_matrix ~branch_length
        ~init:(init, start_state) ~f:(fun (acc, _) n _ -> f acc n, n)
    in
    if Amino_acid.equal simulated_end_state end_state then res
    else loop ()
  in
  loop ()

let nb_events_along_branch_by_rejection_sampling rng ~param ~branch_length ~start_state ~end_state ~sample_size =
  List.init sample_size ~f:(fun _ ->
      simulation_along_branch_by_rejection_sampling
        ~init:0 ~f:(fun acc _ -> acc + 1)
        rng ~param ~branch_length ~start_state ~end_state
    )

let nb_events_along_branch_by_uniformization rng ~param ~branch_length ~start_state ~end_state ~sample_size =
  let process = Phylo_ctmc.Path_sampler.uniformization (AAGTR.rate_matrix param :> Matrix.t) in
  List.init sample_size ~f:(fun _ ->
      Phylo_ctmc.conditional_simulation_along_branch
        rng process ~branch_length
        ~start_state:(Amino_acid.to_int start_state)
        ~end_state:(Amino_acid.to_int end_state)
        ~nstates:Amino_acid.card
      |> Array.length
    )

(* let () =
 *   let start_state = alanine in
 *   let end_state = valine in
 *   let sample_size = 10_000 in
 *   let branch_length = 2. in
 *   printf "Histogram of event nb along a branch for rejection sampling (RS) and uniformized process sampling (UP)\n" ;
 *   printf "RS: " ;
 *   render_int_histogram @@ nb_events_along_branch_by_rejection_sampling rng
 *     ~param:wag_param ~branch_length
 *     ~start_state ~end_state ~sample_size ;
 *   printf "UP: " ;
 *   render_int_histogram @@ nb_events_along_branch_by_uniformization rng
 *     ~param:wag_param ~branch_length
 *     ~start_state ~end_state ~sample_size *)


let vec_of_param (p : AAGTR.t) =
  Array.append
    [| Float.log p.scale |]
    (
      Amino_acid.Table.init (fun aa ->
          Float.log (Amino_acid.Vector.get p.stationary_distribution aa)
        )
      :> float array
    )

let param_of_vec ~exchangeability_matrix vec =
  let scale = Float.exp vec.(0) in
  let stationary_distribution =
    Amino_acid.Vector.init (fun aa ->
        Float.exp vec.(1 + Amino_acid.to_int aa)
      )
    |> Amino_acid.Vector.normalize
  in
  { AAGTR.scale ; stationary_distribution ; exchangeability_matrix }

let string_of_profile p =
  List.filter_map Amino_acid.all ~f:(fun aa ->
      let f_aa = Amino_acid.Vector.get p aa in
      if Float.(f_aa < 0.001) then None
      else Some (sprintf "%c:%.3g" (Amino_acid.to_char aa) f_aa)
    )
  |> String.concat ~sep:" "
  |> sprintf "[%s]"

let string_of_param (p : AAGTR.t) =
  sprintf "{ scale = %f ; pi = %s }" p.scale (string_of_profile p.stationary_distribution)

let pruning site (param : AAGTR.t) =
  let leaf_state = Amino_acid.to_int in
  let root_frequencies = (param.stationary_distribution : Amino_acid.vector :> Vector.t) in
  let transition_matrix = (AAGTR.transition_matrix param :> float -> Matrix.t) in
  Phylo_ctmc.pruning site ~nstates ~transition_matrix ~leaf_state ~root_frequencies

let optimize_pruning site start_param =
  let param_of_vec = param_of_vec ~exchangeability_matrix:start_param.AAGTR.exchangeability_matrix in
  let f vec =
    let param = param_of_vec vec in
    let leaf_state = Amino_acid.to_int in
    let root_frequencies = (param.stationary_distribution : Amino_acid.vector :> Vector.t) in
    let transition_matrix = (AAGTR.transition_matrix param :> float -> Matrix.t) in
    -. Phylo_ctmc.pruning site ~nstates ~transition_matrix ~leaf_state ~root_frequencies
  in
  let theta0 = vec_of_param start_param in
  let sample = nelder_mead_init theta0 in
  let minus_ll, p_star = Nelder_mead.minimize ~tol:1e-3 ~maxit:10_000 ~f ~sample () in
  -. minus_ll, param_of_vec p_star

let sample_site tree param root =
  let rates = AAGTR.rate_matrix param in
  AASim.site_gillespie_first_reaction rng tree ~root ~rate_matrix:(fun _ -> rates)


let iter_branches t ~f =
  let rec traverse_node = function
    | Tree.Leaf _ -> ()
    | Node n ->
      List1.iter n.branches ~f:(traverse_branch n.data)
  and traverse_branch parent_data (Tree.Branch b) =
    f parent_data b.data (Tree.data b.tip) ;
    traverse_node b.tip
  in
  traverse_node t

let sufficient_statistics trees =
  let counts = Array.make_matrix ~dimx:nstates ~dimy:nstates 0 in
  let waiting_times = Array.create ~len:nstates 0. in
  let root_counts = Array.create ~len:nstates 0 in
  Array.iter trees ~f:(fun tree ->
      let root = Tree.data tree in
      root_counts.(root) <- root_counts.(root) + 1 ;
      iter_branches tree ~f:(fun start_state (bl, mapping) _ ->
          match mapping with
          | [||] ->
            waiting_times.(start_state) <- waiting_times.(start_state) +. bl
          | _ ->
            Array.iteri mapping ~f:(fun k (s_j, t_j)  ->
                let s_i, t_i =
                  if k = 0 then start_state, 0.
                  else mapping.(k - 1)
                in
                counts.(s_i).(s_j) <- 1 + counts.(s_i).(s_j) ;
                waiting_times.(s_i) <- waiting_times.(s_i) +. (t_j -. t_i)
              ) ;
            let (last_state, last_time) = Array.last mapping in
            waiting_times.(last_state) <- waiting_times.(last_state) +. bl -. last_time
        )
    ) ;
    counts, waiting_times, root_counts

let mapping_log_likelihood_of_sufficient_statistics param counts waiting_times root_counts =
  let rate_matrix = AAGTR.rate_matrix param in
  let lik = ref 0. in
  for i = 0 to nstates - 1 do
    if root_counts.(i) > 0 then (
      let pi_i = Vector.get (param.stationary_distribution :> Vector.t) i in
      lik := !lik +. float root_counts.(i) *. Float.log pi_i
    ) ;
    lik := !lik +. waiting_times.(i) *. Amino_acid.Matrix.get rate_matrix i i ;
    for j = 0 to nstates - 1 do
      let contrib =
        if i = j then 0.
        else if counts.(i).(j) > 0 then
          float counts.(i).(j) *. Float.log (Amino_acid.Matrix.get rate_matrix i j)
        else 0.
      in
      lik := !lik +. contrib
    done ;
  done ;
  !lik


let mapping_log_likelihood site (param : AAGTR.t) theta =
  let leaf_state = Amino_acid.to_int in
  let root_frequencies = (param.stationary_distribution :> Vector.t) in
  let transition_matrix = (AAGTR.transition_matrix param :> float -> Matrix.t) in
  let sampler =
    let p = Phylo_ctmc.Path_sampler.rejection_sampling (AAGTR.rate_matrix param :> Matrix.t) in
    fun _ -> p
  in
  let conditional_likelihoods =
    Phylo_ctmc.conditionial_likelihoods site ~nstates ~leaf_state ~transition_matrix
  in
  let log_probs =
    Array.init 1_0000 ~f:(fun _ ->
        let conditional_simulation, _prob =
          Phylo_ctmc.conditional_simulation rng ~root_frequencies conditional_likelihoods
        in
        let mapping =
          Phylo_ctmc.substitution_mapping ~rng ~branch_length:Fn.id ~nstates ~sampler conditional_simulation
        in
        let counts, waiting_times, root_frequencies = sufficient_statistics [| mapping |] in
        mapping_log_likelihood_of_sufficient_statistics theta counts waiting_times root_frequencies
      )
  in
  let mean_log_prob = Array.fold log_probs ~init:0. ~f:( +. ) /. float (Array.length log_probs) in
  let _kl_term =
    let probs = Array.map log_probs ~f:Float.exp in
    let sum = Array.fold probs ~f:( +. ) ~init:0. in
    Array.fold probs ~init:0. ~f:(fun acc p_i -> acc +. Float.log (p_i /. sum)) /. float (Array.length probs)
  in
  mean_log_prob (* -. kl_term *)

(* let () =
 *   let tree = sample_tree 5 in
 *   let site = sample_site tree valine in
 *   let pruning_log_lik = pruning site wag_param in
 *   let mapping_log_lik = mapping_log_likelihood site wag_param wag_param in
 *   printf "\n\nLikelihood computation, pruning (P) vs mapping (M)\n" ;
 *   printf "P: %f\n" pruning_log_lik ;
 *   printf "M: %f\n" mapping_log_lik *)

(* let () =
 *   let tree = sample_tree 5 (\* Tree.node  () (List1.singleton (Tree.branch 0.1 @@ Tree.leaf 0)) *\) in
 *   let site = sample_site tree valine in
 *   let ll, p_pruning = optimize_pruning site wag_param in
 *   printf "\n%f %s\n" ll (string_of_param p_pruning) *)

let simulation_probability ~root_frequencies t =
  let rec traverse_tree parent mat = function
    | Tree.Leaf l -> Matrix.get mat parent l
    | Node n ->
      Matrix.get mat parent n.data
      *.
      (
        List1.map n.branches ~f:(traverse_branch n.data)
        |> List1.reduce ~f:( *. )
      )
  and traverse_branch parent (Branch b) =
    traverse_tree parent (snd b.data) b.tip

  and traverse_root = function
    | Tree.Leaf l -> Vector.get root_frequencies l
    | Node n ->
      Vector.get root_frequencies n.data
      *.
      (
        List1.map n.branches ~f:(traverse_branch n.data)
        |> List1.reduce ~f:( *. )
      )
  in
  traverse_root t

type ord = Lt | Eq | Gt

let cmp i j =
  match Int.compare i j with
  |  0 -> Eq
  | -1 -> Lt
  |  1 -> Gt
  | _ -> assert false

let xlogy x y =
  if Float.(x = 0.) then 0. else x *. Float.log y

let _solve param counts waiting_times root_counts =
  let partial_counts =
    (
      Amino_acid.Table.init (fun aa_j ->
          float root_counts.((aa_j :> int)) +. aa_sum ~except:aa_j (fun aa_i -> float counts.((aa_i :> int)).((aa_j :> int)))
        )
      :> float array
    )
  in
  let partial_waiting_times =
    (
      Amino_acid.Table.init (fun aa_j ->
          aa_sum ~except:aa_j (fun aa_i ->
              waiting_times.((aa_i :> int)) *. param.AAGTR.exchangeability_matrix.Amino_acid.%{aa_i, aa_j}
            )
        )
      :> float array
    )
  in
  let total_counts =
    aa_sum (fun aa_i ->
        aa_sum ~except:aa_i (fun aa_j ->
            float counts.((aa_i :> int)).((aa_j :> int))
          )
      )
  in
  let cst =
    aa_sum (fun aa_i ->
        aa_sum ~except:aa_i (fun aa_j ->
            float counts.((aa_i :> int)).((aa_j :> int)) *. log param.AAGTR.exchangeability_matrix.Amino_acid.%{aa_i, aa_j}
          )
      )
  in
  let mean_waiting_times pi =
    aa_sum (fun aa_i ->
        waiting_times.((aa_i :> int))
        *. aa_sum ~except:aa_i (fun aa_j ->
            param.AAGTR.exchangeability_matrix.Amino_acid.%{aa_i, aa_j}
            *. Vector.get pi (aa_j :> int)
          )
      )
  in
  let lagrangian pi sigma lambda =
    total_counts *. Float.log sigma
    +. aa_sum (fun aa_j -> xlogy partial_counts.((aa_j :> int)) (Vector.get pi (aa_j :> int)))
    -. sigma *. mean_waiting_times pi
    -. lambda *. (Vector.sum pi -. 1.)
    +. cst
  in
  let n = Amino_acid.card + 2 in
  let dL_dpi_j pi sigma lambda j =
    partial_counts.(j) /. Vector.get pi j
    -. sigma *. partial_waiting_times.(j) +. lambda
  in
  let dL_dsigma pi sigma =
    total_counts /. sigma -. mean_waiting_times pi
  in
  let dL_dlambda pi = Vector.sum pi -. 1. in
  let gradient pi sigma lambda =
    Vector.init n ~f:(fun j ->
        if j < Amino_acid.card then dL_dpi_j pi sigma lambda j
        else if j = Amino_acid.card then dL_dsigma pi sigma
        else dL_dlambda pi
      )
  in
  (* let d2L_d_pi_j_2 pi j =
   *   -. partial_counts.(j) /. 2. /. ((Vector.get pi j) ** 2.)
   * in *)
  (* let hessian pi sigma =
   *   Matrix.init n ~f:(fun i j ->
   *       match cmp i Amino_acid.card, cmp j Amino_acid.card with
   *       | Lt, Lt ->
   *         if i = j then d2L_d_pi_j_2 pi j else 0.
   *       | Lt, Eq -> -. partial_waiting_times.(i)
   *       | Eq, Lt -> -. partial_waiting_times.(j)
   *       | Eq, Eq -> -. total_counts /. 2. /. (sigma ** 2.)
   *       | Lt, Gt
   *       | Gt, Lt -> 1.
   *       | Eq, Gt
   *       | Gt, Eq
   *       | Gt, Gt -> 0.
   *     )
   * in *)
  let rec loop n pi sigma lambda =
    let obj = lagrangian pi sigma lambda in
    printf "it %d: obj = %f\n" n obj ;
    if n = 0 then ()
    else
      let g = gradient pi sigma lambda in
      print_endline @@ [%show: float array] (Vector.to_array g) ;
      (* Vector.pp Format.std_formatter g ; *)
      (* let h = hessian pi sigma in
       * Matrix.pp Format.std_formatter h ; *)
      (* let delta = Matrix.(apply ((\* inverse *\) h) g) in
       * let pi' = Vector.(init (length pi) ~f:(fun i -> get pi i -. get delta i)) in
       * let sigma' = sigma -. Vector.get delta n in
       * let lambda' = lambda -. Vector.get delta (n + 1) in *)
      let lr = 1e-4 in
      let pi' = Vector.(init (length pi) ~f:(fun i -> get pi i +. lr *. get g i)) in
      let sigma' = sigma +. lr *. Vector.get g n in
      let lambda' = lambda +. lr *. Vector.get g (n + 1) in
      loop (n - 1) pi' sigma' lambda'
  in
  loop 1 (param.stationary_distribution :> Vector.t) param.scale 1.

let _solve2 param counts waiting_times root_counts =
  let partial_counts =
    Amino_acid.Table.init (fun aa_j ->
        float root_counts.((aa_j :> int)) +. aa_sum ~except:aa_j (fun aa_i -> float counts.((aa_i :> int)).((aa_j :> int)))
      )
  in
  let partial_waiting_times =
    Amino_acid.Table.init (fun aa_j ->
        aa_sum ~except:aa_j (fun aa_i ->
            waiting_times.((aa_i :> int)) *. param.AAGTR.exchangeability_matrix.Amino_acid.%{aa_i, aa_j}
          )
      )
  in
  let total_counts =
    aa_sum (fun aa_i ->
        aa_sum ~except:aa_i (fun aa_j ->
            float counts.((aa_i :> int)).((aa_j :> int))
          )
      )
  in
  let cst =
    aa_sum (fun aa_i ->
        aa_sum ~except:aa_i (fun aa_j ->
            float counts.((aa_i :> int)).((aa_j :> int)) *. log param.AAGTR.exchangeability_matrix.Amino_acid.%{aa_i, aa_j}
          )
      )
  in
  let mean_waiting_times pi =
    aa_sum (fun aa_i ->
        waiting_times.((aa_i :> int))
        *. aa_sum ~except:aa_i (fun aa_j ->
            param.AAGTR.exchangeability_matrix.Amino_acid.%{aa_i, aa_j}
            *. Amino_acid.Vector.get pi aa_j
          )
      )
  in
  let obj pi sigma =
    let open Amino_acid in
    total_counts *. Float.log sigma
    +. aa_sum (fun aa_j -> xlogy (Table.get partial_counts aa_j) (Vector.get pi aa_j))
    -. sigma *. mean_waiting_times pi
    +. cst
  in
  let rec loop n pi sigma =
    let obj = obj pi sigma in
    printf "it %d: obj = %f\n" n obj ;
    if n = 0 then { param with stationary_distribution = pi ; scale = sigma }
    else
      let open Amino_acid in
      let pi' =
        Vector.init (fun j ->
            (Table.get partial_counts j) /. sigma /. (Table.get partial_waiting_times j)
          )
        |> Amino_acid.Vector.normalize
      in
      let sigma' = total_counts /. mean_waiting_times pi' in
      print_endline ([%show: float array] (Vector.to_array pi')) ;
      loop (n - 1) pi' sigma'
  in
  loop 1 param.stationary_distribution param.scale

let () =
  let tree = sample_tree 100 in
  let param = sample_param wag in
  let site = sample_site tree param valine in
  let nstates = Amino_acid.card in
  let leaf_state = Amino_acid.to_int in
  let rec loop n ({ AAGTR.stationary_distribution ; _ } as param) =
    if n = 0 then param
    else (
      let root_frequencies = (stationary_distribution : Amino_acid.vector :> Vector.t) in
      let transition_matrix = (AAGTR.transition_matrix param :> float -> Matrix.t) in
      let sampler =
        let p = Phylo_ctmc.Path_sampler.rejection_sampling (AAGTR.rate_matrix param :> Matrix.t) in
        fun _ -> p
      in
      let conditional_likelihoods =
        Phylo_ctmc.conditionial_likelihoods site ~nstates ~leaf_state ~transition_matrix
      in
      let mean_mapping_log_prob, param_hat =
        let mappings, _ =
          Array.init 30 ~f:(fun _ ->
              let conditional_simulation, prob =
                Phylo_ctmc.conditional_simulation rng ~root_frequencies conditional_likelihoods
              in
              Phylo_ctmc.substitution_mapping ~rng ~branch_length:Fn.id ~nstates ~sampler conditional_simulation,
              prob
            )
          |> Array.unzip
        in
        let counts, waiting_times, root_counts = sufficient_statistics mappings in
        (* print_endline ([%show: float array] waiting_times) ;
         * print_endline ([%show: int array] root_counts) ;
         * print_endline ([%show: int array array] counts) ; *)
        let param_hat2 = _solve2 param counts waiting_times root_counts in
        (* let param_hat =
         *   let nu = Amino_acid.Vector.init (fun j ->
         *       let tau_j =
         *         aa_sum (fun i ->
         *             if Amino_acid.equal i j then 0.
         *             else waiting_times.((i :> int)) *. param.exchangeability_matrix.Amino_acid.%{i, j}
         *           )
         *       in
         *       if Float.(tau_j = 0.) then 1.
         *       else
         *         let k_j = float root_counts.((j :> int)) +. aa_sum (fun i -> float counts.((i :> int)).((j :> int))) in
         *         k_j /. tau_j
         *     )
         *   in
         *   let scale_hat = Amino_acid.Vector.sum nu in
         *   let pi_hat = Amino_acid.Vector.normalize nu in
         *   { param with scale = scale_hat ; stationary_distribution = pi_hat }
         * in
         * print_endline (string_of_param param_hat) ;
         * print_endline (string_of_param param_hat2) ; *)
        let mean_mapping_log_prob =
          (
            mapping_log_likelihood_of_sufficient_statistics param_hat2 counts waiting_times root_counts
          ) (* /. float (Array.length mappings) *)
        in
        mean_mapping_log_prob, param_hat2
      in
      (* let marginal_log_prob = 1. *)
        (* Phylo_ctmc.pruning site ~nstates ~transition_matrix:(AAGTR.transition_matrix param_hat :> float -> Matrix.t) ~leaf_state ~root_frequencies *)
      (* in *)
      printf "%f %s\n" mean_mapping_log_prob (string_of_param param_hat) ;
      loop (n - 1) param_hat
    )
  in
  print_endline (
    Tree.leaves site
    |> List.map ~f:Amino_acid.to_char
    |> String.of_char_list
  ) ;
  let ma_time, ma_param = time ~f:(loop 3) wag_param in
  let ma_ll = pruning site ma_param in
  printf "Mapping LL: %f\n" ma_ll ;
  let nm_time, (nm_ll, nm_param) = time ~f:(optimize_pruning site) wag_param in
  printf "%f %s\n" nm_ll (string_of_param nm_param) ;
  printf "Nelder-Mead: %gs\tMapping: %gs\n" nm_time ma_time
