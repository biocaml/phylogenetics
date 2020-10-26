open Core_kernel
open Phylogenetics

module Tdg09 = Phylogenetics_convergence.Tdg09
module Branch_info = struct
  type t = float
  let length x = x
end
module Simulator = Simulator.Make(Amino_acid)(Tdg09.Evolution_model)(Branch_info)

let counts xs =
  Amino_acid.Table.init (fun aa -> List.count xs ~f:(Amino_acid.equal aa))

let multinomial_test freqs counts =
  let _N_ = Array.length freqs in
  let n = Array.fold counts ~init:0 ~f:( + ) in
  let _T_ =
    Array.map2_exn freqs counts ~f:(fun f_i k_i ->
        (float k_i -. float n *. f_i) ** 2. /. (float n *. f_i)
      )
    |> Owl.Stats.sum
  in
  _T_, 1. -. Owl.Stats.chi2_cdf ~df:(float _N_ -. 1.) _T_

let test_stationary_distribution (wag : Wag.t) =
  let p = Tdg09.Evolution_model.param_of_wag wag 1. in
  Amino_acid.Matrix.(zero_eigen_vector (Tdg09.Evolution_model.rate_matrix p)),
  p.stationary_distribution

let stationary_counts_vs_props (wag : Wag.t) ~scale ~nb_leaves ~bl =
  let tree =
    List1.init nb_leaves ~f:(fun _ -> Tree.(branch bl (leaf ())))
    |> Tree.node () 
  in
  let root =
    wag.freqs
    |> Amino_acid.Table.of_vector
    |> Amino_acid.Table.choose
  in
  let p = Tdg09.Evolution_model.param_of_wag wag scale in
  let site = Simulator.site_gillespie_direct tree ~root ~param:(Fn.const p) in
  let leaves = Tree.leaves site in
  let counts = (counts leaves :> int array) in
  let freqs = Amino_acid.Vector.to_array wag.freqs in
  let n = Array.fold counts ~init:0 ~f:( + ) in
  print_endline (Sexp.to_string ([%sexp_of: float array] freqs)) ;
  print_endline (Sexp.to_string ([%sexp_of: float array] (Array.map counts ~f:(fun k_i -> float k_i /. float n)))) ;
  multinomial_test freqs (counts :> int array)

open Alcotest

let testable_amino_acid_vector =
  Alcotest.(testable Amino_acid.Vector.pp) (Amino_acid.Vector.robust_equal ~tol:1e-6)

let test_stationary_distribution () =
  let wag = Wag.parse "../tests/data/wag.dat" in
  let p1, p2 = test_stationary_distribution wag in
  check testable_amino_acid_vector "Stationary distribution" p1 p2

let test_stationary_counts_vs_props () =
  let wag = Wag.parse "../tests/data/wag.dat" in
  let _, pval = stationary_counts_vs_props wag ~scale:1. ~nb_leaves:1000 ~bl:100. in
  check (testable Float.pp Float.( > )) "" pval 1e-4

let tests = [
  "stationary_distribution", `Quick, test_stationary_distribution ;
  "counts_vs_props", `Quick, test_stationary_counts_vs_props ;
]
