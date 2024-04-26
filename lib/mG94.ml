module NSCodon = Codon.Universal_genetic_code.NS

module Nucleotide_rates = Rate_matrix.Nucleotide
module NSCodon_rate_matrix = Rate_matrix.Make(NSCodon)

type param = {
  nucleotide_rates : Rate_matrix.Nucleotide.t ;
  nucleotide_stat_dist : Nucleotide.vector ;
  omega : float ;
}

let rate_matrix { nucleotide_rates ; omega ; _ } =
  let nuc_rates = (nucleotide_rates :> Nucleotide.matrix) in
  NSCodon_rate_matrix.make (fun p q ->
      match NSCodon.neighbours p q with
      | Some (_, x_a, x_b) ->
        let q_ab = nuc_rates.Nucleotide.%{x_a, x_b} in
        if NSCodon.synonym p q then q_ab
        else omega *. q_ab
      | None -> 0.
    )

let stationary_distribution { nucleotide_stat_dist = pi ; _ } =
  let open Nucleotide in
  let pi_a = pi.%(a)
  and pi_g = pi.%(g)
  and pi_t = pi.%(t) in
  let pi_stop =
    pi_t *. pi_a *. pi_g +. pi_t *. pi_g *. pi_a +. pi_t *. pi_a *. pi_a
  in
  let alpha = 1. /. (1. -. pi_stop) in
  NSCodon.Vector.init (fun c ->
      let i, j, k = NSCodon.nucleotides c in
      pi.%(i) *. pi.%(j) *. pi.%(k) *. alpha
    )

let assert_stationary_distributions_are_equal_calculation pi pi' =
  let open Linear_algebra in
  let pi = (pi : NSCodon.vector :> vec) in
  let pi' = (pi' : NSCodon.vector :> vec) in
  let res = Linear_algebra.Vector.robust_equal ~tol:1e-6 pi pi' in
  if not res then (
    Format.eprintf "found: %a\nwanted: %a\n" Linear_algebra.Vector.pp pi Linear_algebra.Vector.pp pi'
  );
  res

let%test "MG94 stationary distribution" =
  let rng = Gsl.Rng.(make (default ())) in
  let nucleotide_process = Nucleotide_process.Random.gtr rng ~alpha:0.3 in
  let nucleotide_rates = Nucleotide_process.rate_matrix nucleotide_process in
  let nucleotide_stat_dist = Nucleotide_process.stationary_distribution nucleotide_process in
  let p = { nucleotide_rates ; nucleotide_stat_dist ; omega = 0.7 } in
  let mat = rate_matrix p in
  assert_stationary_distributions_are_equal_calculation
    (stationary_distribution p)
    (NSCodon_rate_matrix.stationary_distribution mat)
