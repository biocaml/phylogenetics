open Core_kernel
open Phylogenetics_convergence

let lrt_1_vs_2 () =
  let open Tdg09.Implementation_check in
  let wag = Phylogenetics.Wag.parse "wag.dat" in
  lrt_1_vs_2_null_simulation ~mode:`dense ~nb_simulations:100 wag
  |> Fn.flip
    (render_pvalue_histogram ~title:"M1 vs M2 under H0")
    "model1_vs_model2_pvalues_under_h0.pdf"

let lrt_2_vs_3 () =
  let open Tdg09.Implementation_check in
  let wag = Phylogenetics.Wag.parse "wag.dat" in
  lrt_2_vs_3_null_simulation ~nb_simulations:100 ~mode:`sparse wag
  |> Fn.flip
    (render_pvalue_histogram ~title:"M2 vs M3 under H0")
    "model2_vs_model3_pvalues_under_h0.pdf"

let () = lrt_1_vs_2 ()
