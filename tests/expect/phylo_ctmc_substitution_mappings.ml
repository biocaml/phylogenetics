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

module AASim = Simulator.Make(Amino_acid)(Branch_info)

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

let () =
  let tree = sample_tree () in
  let site = sample_site tree valine in
  let nstates = Amino_acid.card in
  let transition_matrix = (transition_matrix wag_param :> float -> Matrix.t) in
  let conditional_simulation =
    Phylo_ctmc.conditionial_likelihoods site ~nstates ~leaf_state:Amino_acid.to_int ~transition_matrix
    |> Phylo_ctmc.conditional_simulation rng ~root_frequencies:(wag.freqs :> Vector.t)
  in
  let process =
    let p = Phylo_ctmc.uniformized_process (rate_matrix wag_param :> Matrix.t) in
    fun _ -> p
  in
  let mapping = Phylo_ctmc.substitution_mapping ~rng ~branch_length:Fn.id ~nstates ~process conditional_simulation in
  let mean_number_of_internal_events =
    let sum, n =
      Tree.prefix_traversal mapping
        ~init:(0., 0)
        ~branch:(fun (sum, n) (bl, history) -> sum +. float (Array.length history) /. bl, n + 1)
        ~node:(fun acc _ -> acc)
        ~leaf:(fun acc _ -> acc)
    in
    sum /. float n
  in
  printf "%f\n" mean_number_of_internal_events
