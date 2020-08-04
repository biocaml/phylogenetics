let () =
  OCamlR_grDevices.pdf "model2_vs_model3_pvalues_under_h0.pdf" ;
  (* Phylogenetics_convergence.Tamuri.lrt_1_vs_2_null_demo (Phylogenetics.Wag.parse "wag.dat") ~sample_size:1 ; *)
  Phylogenetics_convergence.Tamuri.lrt_2_vs_3_null_demo ~debug:false (Phylogenetics.Wag.parse "wag.dat") ~sample_size:1 ;
  (* Phylogenetics_convergence.Tamuri.lrt_null_demo ~mode:`dense ~sample_size:50 (Phylogenetics.Wag.parse "wag.dat") ; *)
  OCamlR_grDevices.dev_off ()
;;
