let () =
  OCamlR_grDevices.pdf "rien.pdf" ;
  Phylogenetics_convergence.Tamuri.lrt_null_demo (Phylogenetics.Wag.parse "wag.dat") ;
  OCamlR_grDevices.dev_off ()
;;
