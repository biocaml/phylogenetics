open Biocaml_phylogeny_test

let time f =
    let t = Sys.time() in
    let fexec = f () in
    Printf.printf "\027[0;31mExecution time: %fs\027[0;0m\n" (Sys.time() -. t);
    fexec

let f () = Printf.printf "Hello world!\n"

let _ = time f
