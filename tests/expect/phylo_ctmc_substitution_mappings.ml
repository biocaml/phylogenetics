open Core
open Phylogenetics
open Phylogenetics.Linear_algebra

let rng = Gsl.Rng.(make (default ()))

let sample_tree () =
  let bdp = Birth_death.make ~birth_rate:2. ~death_rate:1. in
  Birth_death.age_ntaxa_simulation bdp rng ~age:1. ~ntaxa:100

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

let transition_rates p =
  Rate_matrix.Amino_acid.make (substitution_rate p)

let transition_probabilities_of_rates m =
  fun bl -> (Amino_acid.Matrix.(expm (scal_mul bl m)) :> mat)

let transition_matrix p =
  let m = transition_rates p in
  transition_probabilities_of_rates m

let uniformized_process p =
  let transition_rates = transition_rates p in
  let transition_probabilities =
    transition_probabilities_of_rates transition_rates
  in
  Phylo_ctmc.Uniformized_process.make
    ~transition_rates:(transition_rates :> mat)
    ~transition_probabilities

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

let nb_events_along_branch rng path_sampler ~start_state ~end_state ~sample_size =
  List.init sample_size ~f:(fun _ ->
      Phylo_ctmc.Path_sampler.sample_exn
        ~rng path_sampler
        ~start_state:(Amino_acid.to_int start_state)
        ~end_state:(Amino_acid.to_int end_state)
      |> Array.length
    )

let () =
  let start_state = alanine in
  let end_state = valine in
  let sample_size = 10_000 in
  let branch_length = 2. in
  let process = Staged.unstage (uniformized_process wag_param) ~branch_length in
  let uniformized_path_sampler = Phylo_ctmc.Path_sampler.uniformization process in
  let rejection_path_sampler = Phylo_ctmc.Path_sampler.rejection_sampling ~rates:(Phylo_ctmc.Uniformized_process.transition_rates process) ~branch_length () in
  render_int_histogram (
    nb_events_along_branch
      rng rejection_path_sampler
      ~start_state ~end_state ~sample_size
  ) ;
  render_int_histogram (
    nb_events_along_branch
      rng uniformized_path_sampler
      ~start_state ~end_state ~sample_size
  )

let sample_site tree root =
  let rates = transition_rates wag_param in
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

let sufficient_statistics ~nstates tree =
  let counts = Array.make_matrix ~dimx:nstates ~dimy:nstates 0 in
  let waiting_times = Array.create ~len:nstates 0. in
  iter_branches tree ~f:(fun start_state (bl, mapping) _ ->
      match mapping with
      | [||] -> ()
      | _ ->
        Array.iteri mapping ~f:(fun k (s_j, t_j)  ->
            let s_i, t_i =
              if k = 0 then start_state, 0.
              else mapping.(k -1)
            in
            counts.(s_i).(s_j) <- 1 + counts.(s_i).(s_j) ;
            waiting_times.(s_i) <- waiting_times.(s_i) +. (t_j -. t_i)
          ) ;
        let (last_state, last_time) = Array.last mapping in
        waiting_times.(last_state) <- waiting_times.(last_state) +. bl -. last_time
    ) ;
  counts, waiting_times

let mapping_likelihood ~nstates ~transition_rates tree =
  let counts, waiting_times = sufficient_statistics ~nstates tree in
  let lik = ref 0. in
  for i = 0 to nstates - 1 do
    for j = 0 to nstates - 1 do
      let contrib =
        if i = j then waiting_times.(i) *. Matrix.get transition_rates i i
        else float counts.(i).(j) *. Float.log (Matrix.get transition_rates i j)
      in
      lik := !lik +. contrib
    done ;
  done ;
  !lik

let () =
  let tree = sample_tree () in
  let site = sample_site tree valine in
  let nstates = Amino_acid.card in
  let leaf_state (_, aa) = Amino_acid.to_int aa in
  let root_frequencies =  (wag.freqs :> Vector.t) in
  let transition_rates = transition_rates wag_param in
  let conditional_likelihoods =
    let transition_probabilities bl = transition_probabilities_of_rates transition_rates bl in
    Phylo_ctmc.conditional_likelihoods site ~nstates ~leaf_state ~transition_probabilities
  in
  let process = Staged.unstage (uniformized_process wag_param) in
  let path_sampler bi = Phylo_ctmc.Path_sampler.uniformization (process ~branch_length:bi) in
  let mean_mapping_likelihood =
    Array.init 100 ~f:(fun _ ->
        Phylo_ctmc.conditional_simulation rng ~root_frequencies conditional_likelihoods
        |> Phylo_ctmc.substitution_mapping ~rng ~path_sampler
        |> mapping_likelihood ~nstates ~transition_rates:(transition_rates :> mat)
      )
    |> Gsl.Stats.mean
  in
  let pruning_likelihood =
    let transition_probabilities bl = [`Mat (transition_probabilities_of_rates transition_rates bl)] in
    Phylo_ctmc.pruning site ~nstates ~transition_probabilities ~leaf_state ~root_frequencies
  in
  printf "%f %f\n" pruning_likelihood mean_mapping_likelihood
