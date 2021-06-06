open Core_kernel
open Phylogenetics
open Phylogenetics.Linear_algebra.Lacaml

let rng = Gsl.Rng.(make (default ()))

let sample_tree () =
  let bdp = Birth_death.make ~birth_rate:2. ~death_rate:1. in
  Birth_death.age_ntaxa_simulation bdp rng ~age:1. ~ntaxa:5
  (* Tree.node  () (List1.singleton (Tree.branch 1. @@ Tree.leaf 0)) *)

let sample_profile () =
  let disp = 1. in
  Amino_acid.Vector.init (fun _ ->
      Gsl.Randist.gamma rng ~a:(1. /. disp) ~b:(1. /. disp)
    )
  |> Amino_acid.Vector.normalize

module Branch_info = struct
  type t = float
  let length x = x
end

module AASim = Phylogenetics.Simulator.Make(Amino_acid)(Branch_info)

let valine = Option.value_exn (Amino_acid.of_char 'V')
let alanine = Option.value_exn (Amino_acid.of_char 'A')

let wag = Wag.parse "../data/wag.dat"

type param = {
  stationary_distribution : Amino_acid.vector ;
  exchangeability_matrix : Amino_acid.matrix ;
  scale : float ;
}

let substitution_rate p i j =
  p.scale *.
  p.exchangeability_matrix.Amino_acid.%{i, j} *.
  p.stationary_distribution.Amino_acid.%(j)

let rate_matrix p =
  Rate_matrix.Amino_acid.make (substitution_rate p)

let transition_matrix p =
  let m = rate_matrix p in
  fun bl -> Amino_acid.Matrix.(expm (scal_mul bl m))

let wag_param = {
  stationary_distribution = wag.freqs ;
  exchangeability_matrix = wag.rate_matrix ;
  scale = 1. ;
}

(* Test on single branch *)

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
  let rate_matrix = rate_matrix param in
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

let nb_events_along_branch rng ~param ~branch_length ~start_state ~end_state ~sample_size =
  let process = Phylo_ctmc.uniformized_process (rate_matrix param :> Matrix.t) in
  List.init sample_size ~f:(fun _ ->
      Phylo_ctmc.conditional_simulation_along_branch
        rng process ~branch_length
        ~start_state:(Amino_acid.to_int start_state)
        ~end_state:(Amino_acid.to_int end_state)
        ~nstates:Amino_acid.card
      |> Array.length
    )

let () =
  let start_state = alanine in
  let end_state = valine in
  let sample_size = 10_000 in
  let branch_length = 2. in
  render_int_histogram @@ nb_events_along_branch_by_rejection_sampling rng
    ~param:wag_param ~branch_length
    ~start_state ~end_state ~sample_size ;
  render_int_histogram @@ nb_events_along_branch rng
    ~param:wag_param ~branch_length
    ~start_state ~end_state ~sample_size

let sample_site tree root =
  let rates = rate_matrix wag_param in
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

let sufficient_statistics ~nstates trees =
  let counts = Array.make_matrix ~dimx:nstates ~dimy:nstates 0 in
  let waiting_times = Array.create ~len:nstates 0. in
  Array.iter trees ~f:(fun tree ->
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
    counts, waiting_times

let mapping_log_likelihood_aux ~nstates ~rate_matrix counts waiting_times =
  let lik = ref 0. in
  for i = 0 to nstates - 1 do
    for j = 0 to nstates - 1 do
      let contrib =
        if i = j then waiting_times.(i) *. Matrix.get rate_matrix i i
        else float counts.(i).(j) *. Float.log (Matrix.get rate_matrix i j)
      in
      lik := !lik +. contrib
    done ;
  done ;
  !lik

let mapping_log_likelihood ~nstates ~rate_matrix tree =
  let counts, waiting_times = sufficient_statistics ~nstates tree in
  mapping_log_likelihood_aux ~nstates ~rate_matrix counts waiting_times

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

let aa_sum f = List.fold Amino_acid.all ~init:0. ~f:(fun acc i -> acc +. f i)

let () =
  let tree = sample_tree () in
  let site = sample_site tree valine in
  let nstates = Amino_acid.card in
  let leaf_state = Amino_acid.to_int in
  let rec loop n scale stationary_distribution =
    if n = 0 then ()
    else (
      let root_frequencies = (stationary_distribution : Amino_acid.vector :> Vector.t) in
      let param = {
        exchangeability_matrix = wag.rate_matrix ;
        stationary_distribution ; scale ;
      }
      in
      let transition_matrix = (transition_matrix param :> float -> Matrix.t) in
      let process =
        let p = Phylo_ctmc.uniformized_process (rate_matrix param :> Matrix.t) in
        fun _ -> p
      in
      let conditional_likelihoods =
        Phylo_ctmc.conditionial_likelihoods site ~nstates ~leaf_state ~transition_matrix
      in
      let mean_full_log_prob =
        Array.init 1_000 ~f:(fun _ ->
            let conditional_simulation, _ =
              Phylo_ctmc.conditional_simulation rng ~root_frequencies conditional_likelihoods
            in
            simulation_probability ~root_frequencies conditional_simulation
            |> Float.log
          )
        |> Gsl.Stats.mean
      in
      let marginal_log_prob =
        Phylo_ctmc.pruning site ~nstates ~transition_matrix ~leaf_state ~root_frequencies
      in
      let mean_mapping_log_prob, mean_mapping_log_prob_opt, scale_hat, pi_hat =
        let mappings, probs =
          Array.init 1_000 ~f:(fun _ ->
              let conditional_simulation, prob =
                Phylo_ctmc.conditional_simulation rng ~root_frequencies conditional_likelihoods
              in
              Phylo_ctmc.substitution_mapping ~rng ~branch_length:Fn.id ~nstates ~process conditional_simulation,
              prob
            )
          |> Array.unzip
        in
        let counts, waiting_times = sufficient_statistics ~nstates mappings in
        let mean_mapping_log_prob_opt, scale_hat, pi_hat =
          let nu = Amino_acid.Vector.init (fun _j ->
              aa_sum (fun i -> waiting_times.((i :> int)) (* *. param.exchangeability_matrix.Amino_acid.%{i, j} *))
              /. aa_sum (fun _i -> (* float counts.((i :> int)).((j :> int)) *) float 1)
            )
          in
          if true then exit 0 ;
          let scale_hat = Amino_acid.Vector.sum nu in
          let pi_hat = Amino_acid.Vector.normalize nu in
          (* let rate_matrix_hat = (rate_matrix { param with scale = scale_hat ; stationary_distribution = pi_hat } :> Matrix.t) in *)
          (* mapping_log_likelihood_aux ~nstates ~rate_matrix:rate_matrix_hat counts waiting_times,
           * scale_hat,
           * pi_hat *)
          42., scale_hat, pi_hat
        in
        let mean_mapping_log_prob =
          let rate_matrix = (rate_matrix { param with stationary_distribution } :> Matrix.t) in
          (
            mapping_log_likelihood_aux ~nstates ~rate_matrix counts waiting_times
            +. Array.fold probs ~init:0. ~f:(fun acc p -> acc +. Float.log p)
          ) /. float (Array.length mappings)
        in
        mean_mapping_log_prob, mean_mapping_log_prob_opt, scale_hat, pi_hat
      in
      printf "%f %f %f %f\n" marginal_log_prob mean_full_log_prob mean_mapping_log_prob mean_mapping_log_prob_opt ; 
      loop (n - 1) scale_hat pi_hat
    )
  in
  print_endline (
    Tree.leaves site
    |> List.map ~f:Amino_acid.to_char
    |> String.of_char_list
  ) ;
  loop 5 1. wag.freqs
