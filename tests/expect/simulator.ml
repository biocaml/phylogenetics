open Core
open Phylogenetics

module L = Linear_algebra

module Branch_info = struct
  type t = float
  let length x = x
end

module Model = struct
  type param = {
    stationary_distribution : Amino_acid.vector ;
    exchangeability_matrix : Amino_acid.matrix ;
  }

  let substitution_rate p i j =
    p.exchangeability_matrix.Amino_acid.%{i, j} *.
    p.stationary_distribution.Amino_acid.%(j)

  let rate_matrix p =
    Rate_matrix.Amino_acid.make (substitution_rate p)
end

module Sim = Simulator.Make(Amino_acid)(Branch_info)

let single_branch_tree l =
  Tree.node () List1.(cons (Tree.branch l (Tree.leaf ())) [])

let draw_amino_acid_profile rng alpha =
  let theta = Array.create ~len:20 0. in
  Gsl.Randist.dirichlet rng ~alpha:(Array.create ~len:20 alpha) ~theta ;
  Amino_acid.Vector.of_array_exn theta

let simulation_on_one_branch simulator simulator_name =
  let rng = Gsl.Rng.(make (default ())) in
  let tree = single_branch_tree 10. in
  let root = Amino_acid.of_int_exn 0 in
  let target = draw_amino_acid_profile rng 0.1 in
  let exchangeability_matrix = (Wag.from_file_exn "../data/wag.dat").rate_matrix in
  let param = {
    Model.stationary_distribution = target ;
    exchangeability_matrix ;
  }
  in
  let rates = Model.rate_matrix param in
  let empirical_frequencies =
    Sequence.init 1000 ~f:(fun _ ->
        simulator rng tree ~root ~rate_matrix:(Fn.const rates)
        |> Tree.leaves
        |> List.map ~f:snd
        |> List.hd_exn
      )
    |> Amino_acid.counts
    |> (fun (k : int Amino_acid.table) -> (k :> int array))
    |> Array.map ~f:Float.of_int
    |> Amino_acid.Vector.of_array_exn
    |> Amino_acid.Vector.normalize
  in
  let res =
    [|
      Amino_acid.Vector.to_array target ;
      Amino_acid.Vector.to_array empirical_frequencies ;
    |]
    |> L.Matrix.of_arrays_exn
    |> L.Matrix.transpose
  in
  printf "Test convergence of %s on one branch\n" simulator_name ;
  L.Matrix.pp Format.std_formatter res ;
  Stdlib.print_newline ()

let () =
  simulation_on_one_branch Sim.site_gillespie_direct "Gillespie direct" ;
  simulation_on_one_branch Sim.site_gillespie_first_reaction "Gillespie first reaction"
