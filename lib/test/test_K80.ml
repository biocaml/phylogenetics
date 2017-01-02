open Biocaml_phylogeny_core
open Alcotest

module K80 = Models.K80
module Utils = Model_utils.Model_utils (K80)
open LATools

let test () =
  Utils.eMt 2.0 0.1
  |> pp_mat Format.std_formatter

let test2 () =
  Utils.eMt_series 2.0 0.1
  |> pp_mat Format.std_formatter
