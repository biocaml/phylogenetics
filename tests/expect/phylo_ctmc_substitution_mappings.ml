open Core_kernel
open Phylogenetics
open Phylogenetics.Linear_algebra.Lacaml

let rng = Gsl.Rng.(make (default ()))

let sample_tree () =
  let bdp = Birth_death.make ~birth_rate:2. ~death_rate:1. in
  Birth_death.age_ntaxa_simulation bdp rng ~age:1. ~ntaxa:5

module Branch_info = struct
  type t = float
  let length x = x
end

module AASim = Phylogenetics.Simulator.Make(Amino_acid)(Branch_info)

let valine = Option.value_exn (Amino_acid.of_char 'V')

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

let mapping_likelihood ~nstates ~rate_matrix tree =
  let counts, waiting_times = sufficient_statistics ~nstates tree in
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

let () =
  let tree = sample_tree () in
  let site = sample_site tree valine in
  let nstates = Amino_acid.card in
  let transition_matrix = (transition_matrix wag_param :> float -> Matrix.t) in
  let leaf_state = Amino_acid.to_int in
  let root_frequencies =  (wag.freqs :> Vector.t) in
  let conditional_likelihoods =
    Phylo_ctmc.conditionial_likelihoods site ~nstates ~leaf_state ~transition_matrix
  in
  let rate_matrix = (rate_matrix wag_param :> Matrix.t) in
  let process =
    let p = Phylo_ctmc.uniformized_process rate_matrix in
    fun _ -> p
  in
  let mean_mapping_likelihood =
    Array.init 1_000 ~f:(fun _ ->
        Phylo_ctmc.conditional_simulation rng ~root_frequencies conditional_likelihoods
        |> Phylo_ctmc.substitution_mapping ~rng ~branch_length:Fn.id ~nstates ~process
        |> mapping_likelihood ~nstates ~rate_matrix
      )
    |> Gsl.Stats.mean
  in
  printf "%f %f\n"
    (Phylo_ctmc.pruning site ~nstates ~transition_matrix ~leaf_state ~root_frequencies)
    mean_mapping_likelihood
