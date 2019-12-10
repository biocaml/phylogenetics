open Core_kernel
module Convsim = Simulator
open Phylogenetics
open Phylogenetics.Linear_algebra.Lacaml


module Evolution_model = struct
  type param = {
    stationary_distribution : Amino_acid.vector ;
    exchangeability_matrix : Amino_acid.matrix ;
    scale : float ;
  }
  let param_of_wag (wag : Wag.t) scale = {
    scale ;
    stationary_distribution = wag.freqs ;
    exchangeability_matrix = wag.rate_matrix ;
  }
  let stationary_distribution p = p.stationary_distribution
  let rate_matrix p =
    Rate_matrix.Amino_acid.make (fun i j ->
        p.scale *.
        p.exchangeability_matrix.Amino_acid.%{i, j} *.
        p.stationary_distribution.Amino_acid.%(j)
      )
  let transition_probability_matrix p t =
    Amino_acid.Matrix.(expm (scal_mul t (rate_matrix p)))
end

module Simulator = Simulator.Make(Amino_acid)(Evolution_model)

let model1_maximum_likelihood ~exchangeability_matrix ~stationary_distribution site =
  let module CTMC = Phylo_ctmc.Make(Amino_acid) in
  let pi = (stationary_distribution : Amino_acid.vector :> vec) in
  let f param =
    let p = { Evolution_model.scale = param.(0) ;
              exchangeability_matrix ;
              stationary_distribution } in
    let transition_matrix =
      let f = Evolution_model.transition_probability_matrix p in
      fun (bl, _) -> (f bl :> mat)
    in
    CTMC.pruning site ~transition_matrix ~leaf_state:Fn.id ~root_frequencies:pi
  in
  let sample () = [| Owl.Stats.uniform_rvs ~a:0. ~b:10. |] in
  let ll, p_star = Nelder_mead.minimize ~debug:true ~maxit:10 ~f ~sample () in
  ll, p_star.(0)

let model1_demo (wag : Wag.t) =
  let tree =
    Convsim.pair_tree ~branch_length1:1. ~branch_length2:1. ~npairs:6
    |> Tree.map ~node:Fn.id ~leaf:Fn.id ~branch:(fun bi ->
        Option.value_exn bi.length,
        List.Assoc.find_exn ~equal:String.equal bi.tags "Condition"
      )
  in
  let root =
    wag.freqs
    |> Amino_acid.Table.of_vector
    |> Amino_acid.Table.choose
  in
  let true_scale = 1. in
  let p = Evolution_model.param_of_wag wag true_scale in
  let site = Simulator.site_gillespie_direct tree ~root ~param:(Fn.const p) in
  let ll, scale_star =
    model1_maximum_likelihood
      ~exchangeability_matrix:wag.rate_matrix
      ~stationary_distribution:wag.freqs
      site
  in
  true_scale, scale_star, ll
