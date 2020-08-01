let () =
  OCamlR_grDevices.pdf "rien.pdf" ;
  (* Phylogenetics_convergence.Tamuri.lrt_1_vs_2_null_demo (Phylogenetics.Wag.parse "wag.dat") ~sample_size:1 ; *)
  Phylogenetics_convergence.Tamuri.lrt_2_vs_3_null_demo (Phylogenetics.Wag.parse "wag.dat") ~sample_size:1 ;
  (* Phylogenetics_convergence.Tamuri.lrt_null_demo ~sample_size:50 (Phylogenetics.Wag.parse "wag.dat") ; *)
  OCamlR_grDevices.dev_off ()
;;
