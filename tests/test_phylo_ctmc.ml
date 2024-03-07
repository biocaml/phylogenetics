open Core
open Phylogenetics

let test_pruning ?(tree_size = 5) ?(seq_size = 10) () =
  let module M = Site_evolution_model.JC69 in
  let module Align = Alignment.Make(Seq.DNA) in
  let module F = Felsenstein.Make(Nucleotide)(Align)(M) in
  let module SG = Sequence_generation.Make(Nucleotide)(Seq.DNA)(Align)(M) in
  let module CTMC = Phylo_ctmc in
  let tree = Phylogenetic_tree.make_random tree_size in
  let align =
    SG.seqgen_string_list () tree seq_size
    |> Align.of_string_list
  in
  let felsenstein_result = F.felsenstein () tree align in
  let ctmc_result =
    let tree = Phylogenetic_tree.to_tree tree in
    let transition_probabilities l = [`Mat (M.transition_probability_matrix () l :> Linear_algebra.mat)] in
    let root_frequencies = (M.stationary_distribution () :> Linear_algebra.vec) in
    Array.init (Align.length align) ~f:(fun i ->
        let leaf_state (_, index) = Align.get_base align ~seq:index ~pos:i |> Nucleotide.to_int in
        CTMC.pruning tree ~nstates:Nucleotide.card ~transition_probabilities ~leaf_state ~root_frequencies
      )
    |> Utils.array_sum
  in
  Test_utils.check_likelihood felsenstein_result ctmc_result

let vec_of_sv (Phylo_ctmc.SV (v, carry)) =
  Linear_algebra.Vector.scal_mul (Float.exp carry) v

let check_equal_clv v1 v2 =
  let open Linear_algebra in
  let d2 = Vector.(
      let delta = add v1 (scal_mul (-1.) v2) in
      sum (mul delta delta)
    )
  in
  if Float.(sqrt d2 < 1e-6) then Ok ()
  else Error (`Different_conditional_likelihoods (v1, v2))

let rec check_equal_conditional_likelihood cl1 cl2 =
  let open Tree in
  let open Result.Monad_infix in
  match cl1, cl2 with
  | Leaf _, Leaf _ -> Ok ()
  | Node n1, Node n2 -> (
      let v1 = vec_of_sv n1.data and v2 = vec_of_sv n2.data in
      check_equal_clv v1 v2 >>= fun () ->
      match List1.map2 n1.branches n2.branches ~f:(fun (Branch b1) (Branch b2) ->
          check_equal_conditional_likelihood b1.tip b2.tip
        )
      with
      | Ok results -> Result.all_unit (List1.to_list results)
      | Error `Unequal_lengths -> Error `Different_structure
    )
  | _ -> Error `Different_structure

let test_conditional_likelihood_ambiguity () =
  let module BI = struct type t = float let length x = x end in
  let module Sim = Simulator.Make(Amino_acid)(BI) in
  let rng = Gsl.Rng.(make (default ())) in
  let bd = Birth_death.make ~birth_rate:1. ~death_rate:0.9 in
  let tree = Birth_death.age_ntaxa_simulation bd rng ~age:1. ~ntaxa:20 in
  let stationary_distribution = Amino_acid.random_profile rng 0.1 in
  let root_state = Amino_acid.of_char_exn 'V' in
  let rate_matrix = Rate_matrix.Amino_acid.gtr
      ~stationary_distribution
      ~exchangeabilities:(Rate_matrix.Amino_acid.make_symetric (fun _ _ -> 1e-3))
  in
  let site = Sim.site_gillespie_direct rng ~root:root_state tree ~rate_matrix:(Fun.const rate_matrix) in
  let nstates = Amino_acid.card in
  let transition_probabilities bl =
    (Amino_acid.Matrix.(expm (scal_mul bl rate_matrix)) :> Linear_algebra.mat)
  in
  let cl = Phylo_ctmc.conditional_likelihoods site ~nstates
      ~transition_probabilities
      ~leaf_state:(fun (_, aa) -> Amino_acid.to_int aa) in
  let cl_amb = Phylo_ctmc.Ambiguous.conditional_likelihoods site ~nstates
      ~leaf_state:(fun (_, aa) i -> Amino_acid.to_int aa = i)
      ~transition_probabilities
  in
  match check_equal_conditional_likelihood cl cl_amb with
  | Ok () -> ()
  | Error (`Different_conditional_likelihoods (v1, v2)) ->
    let open Linear_algebra in
    failwithf
      "Different conditional likelihoods:\n%s\n%s\n%s"
      ([%show: float array] (Amino_acid.Vector.to_array stationary_distribution))
      ([%show: float array] (Vector.to_array v1))
      ([%show: float array] (Vector.to_array v2))
      ()

  | Error `Different_structure -> failwith "Different tree structure"

let tests = [
  ("Felsenstein vs Phylo_ctmc", `Quick, test_pruning ~tree_size:100 ~seq_size:10) ;
  ("Cond. lik. simple vs ambiguous", `Quick, test_conditional_likelihood_ambiguity) ;
]
