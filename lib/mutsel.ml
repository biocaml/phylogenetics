open Core_kernel

module NSCodon = Codon.Universal_genetic_code.NS

module Nucleotide_rates = Rate_matrix.Nucleotide
module Amino_acid_rates = Rate_matrix.Make(Amino_acid)
module NSCodon_rate_matrix = Rate_matrix.Make(NSCodon)

type param = {
  nucleotide_rates : Rate_matrix.Nucleotide.t ;
  nucleotide_stat_dist : Nucleotide.vector ;
  omega : float ; (* dN/dS *)
  scaled_fitness : Amino_acid.vector ;
  gBGC : float ;
}

let fitness_of_profile ?(beta = 1.) =
  Amino_acid.Vector.map ~f:(fun x -> beta *. Float.log x)

let flat_fitness () =
  Amino_acid.Vector.init (fun _ -> 1. /. float Amino_acid.card)
  |> fitness_of_profile

let random_param ~alpha_nucleotide ~alpha_fitness =
  let pi = Nucleotide.random_profile alpha_nucleotide in
  let rho = Utils.random_profile 6 in
  let nucleotide_rates = Nucleotide_rates.gtr ~equilibrium_frequencies:pi ~transition_rates:rho in
  {
    nucleotide_rates ;
    nucleotide_stat_dist = pi ;
    omega = 1. ;
    scaled_fitness =
      Amino_acid.random_profile alpha_fitness
      |> fitness_of_profile ;
    gBGC = 0. ;
  }

let flat_param () =
  let pi = Nucleotide.flat_profile () in
  let rho = Linear_algebra.Vector.init 6 ~f:(fun _ -> 1. /. 6.) in
  let nucleotide_rates = Nucleotide_rates.gtr ~equilibrium_frequencies:pi ~transition_rates:rho in
  {
    nucleotide_rates ;
    nucleotide_stat_dist = pi ;
    omega = 1. ;
    scaled_fitness = flat_fitness () ;
    gBGC = 0. ;
  }


let fixation_probability delta =
  let open Float in
  if abs delta < 1e-30 then 1. + delta / 2.
  else if delta > 50. then delta
  else if delta < - 50. then 0.
  else delta / (1. - exp (- delta))

let rate_matrix { nucleotide_rates ; omega ; scaled_fitness = _F_ ; gBGC ; _ } =
  let nuc_rates = (nucleotide_rates :> Nucleotide.matrix) in
  NSCodon_rate_matrix.make (fun p q ->
      match NSCodon.neighbours p q with
      | Some (_, x_a, x_b) ->
        let _B_ = match Nucleotide.(inspect x_a, inspect x_b) with
          | (A | T), (C | G) -> gBGC
          | (C | G), (A | T) -> -. gBGC
          | _ -> 0.
        in
        let selection_coefficient =
          _B_ +.
          if NSCodon.synonym p q then 0.
          else
            let aa_p = NSCodon.aa_of_codon p in
            let aa_q = NSCodon.aa_of_codon q in
            _F_.Amino_acid.%(aa_q) -. _F_.Amino_acid.%(aa_p)
        in
        let p_fix = fixation_probability selection_coefficient in
        let q_ab = nuc_rates.Nucleotide.%{x_a, x_b} in
        q_ab *. omega *. p_fix
      | None -> 0.
    )

let stationary_distribution p =
  let pi = p.nucleotide_stat_dist in
  NSCodon.Vector.init (fun codon ->
      let n1, n2, n3 = NSCodon.nucleotides codon in
      let aa = NSCodon.aa_of_codon codon in
      let b n = match Nucleotide.inspect n with
        | A | T -> -. p.gBGC
        | C | G -> +. p.gBGC
      in
      Nucleotide.(pi.%(n1) *. pi.%(n2) *. pi.%(n3))
      *. exp (p.scaled_fitness.Amino_acid.%(aa) +. (b n1 +. b n2 +. b n3) /. 2.)
    )
  |> NSCodon.Vector.normalize

let transition_probability_matrix p t =
  NSCodon.Matrix.(expm (scal_mul t (rate_matrix p)))

(* == TESTS ================================================================= *)

let test_stationary_distribution_sums_to_one stationary_distribution =
  let p = random_param ~alpha_nucleotide:10. ~alpha_fitness:10. in
  let pi = (stationary_distribution p : NSCodon.vector) in
  Utils.robust_equal 1. (NSCodon.Vector.sum pi)

let stationary_distribution_by_linear_resolution p =
  NSCodon_rate_matrix.stationary_distribution (rate_matrix p)

let%test "Codon model stationary distribution sums to one" =
  test_stationary_distribution_sums_to_one stationary_distribution

let%test "Codon model stationary distribution sums to one (linear resolution)" =
  test_stationary_distribution_sums_to_one stationary_distribution_by_linear_resolution

let test_stationary_distribution_for_flat_parameters_is_uniform stationary_distribution =
  let p = flat_param () in
  let pi = (stationary_distribution p : NSCodon.vector :> Owl.Arr.arr) in
  let res = Owl.Arr.for_all (fun x -> Float.robustly_compare x (1. /. 61.) = 0) pi in
  if not res then
    fprintf stderr "stationary distribution = %s\n" (Utils.show_float_array (Owl.Arr.to_array pi)) ;
  res

let%test "Codon model stationary distribution for flat parameters is uniform" =
  test_stationary_distribution_for_flat_parameters_is_uniform stationary_distribution

let%test "Codon model stationary distribution for flat parameters is uniform" =
  test_stationary_distribution_for_flat_parameters_is_uniform stationary_distribution_by_linear_resolution

let test_stationary_distribution_with_flat_nucleotidic_parameters_has_equal_frequency_for_synonyms stationary_distribution =
  let p = random_param ~alpha_nucleotide:10. ~alpha_fitness:10. in
  let p_flat = flat_param () in
  let p = { p with nucleotide_rates = p_flat.nucleotide_rates ;
                   nucleotide_stat_dist = p_flat.nucleotide_stat_dist } in
  let pi = stationary_distribution p in
  List.for_all NSCodon.all ~f:(fun a ->
      List.for_all NSCodon.all ~f:(fun b ->
          not (NSCodon.synonym a b) || NSCodon.(Utils.robust_equal pi.%(a) pi.%(b))
        )
    )

let%test "Codon model stationary distribution with flat nucleotidic parameters has equal frequency for synonyms" =
  test_stationary_distribution_with_flat_nucleotidic_parameters_has_equal_frequency_for_synonyms stationary_distribution

let%test "Codon model stationary distribution with flat nucleotidic parameters has equal frequency for synonyms (linear resolution)" =
  test_stationary_distribution_with_flat_nucleotidic_parameters_has_equal_frequency_for_synonyms stationary_distribution_by_linear_resolution

let test_both_stationary_distribution_calculation p =
  let pi = (stationary_distribution p :> Owl.Arr.arr) in
  let pi' = (NSCodon_rate_matrix.stationary_distribution (rate_matrix p) :> Owl.Arr.arr) in
  let res = Owl.Arr.approx_equal pi pi' in
  if not res then (
    let pi = Owl.Arr.to_array pi in
    let pi' = Owl.Arr.to_array pi' in
    fprintf stderr "found: %s\nwanted: %s\n" (Utils.show_float_array pi) (Utils.show_float_array pi')
  );
  res

let%test "Codon model stationary distribution calculation" =
  let p = random_param ~alpha_nucleotide:10. ~alpha_fitness:10. in
  test_both_stationary_distribution_calculation p

let%test "Codon model stationary distribution calculation with gBGC" =
  let p = random_param ~alpha_nucleotide:10. ~alpha_fitness:10. in
  let p = { p with gBGC = 0.2345 } in
  test_both_stationary_distribution_calculation p
