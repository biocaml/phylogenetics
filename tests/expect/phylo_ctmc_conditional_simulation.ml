open Core
open Phylogenetics

let rng = Gsl.Rng.(make (default ()))
let _B_ = 1_000

let small_tree =
  let open Tree in
  binary_node ()
    (branch 0.4 (
        binary_node ()
          (branch 0.8 (leaf "A1"))
          (branch 1.2 (leaf "A2"))))
    (branch 0.1 (
        binary_node ()
          (branch 0.6 (leaf "B"))
          (branch 0.3 (leaf "C"))))

module Branch_info = struct
  type t = float
  let length x = x
end

module NucSim = Phylogenetics.Simulator.Make(Nucleotide)(Branch_info)
module SeqSim = Sequence_simulator.Make(Nucleotide)

let reference_simulation, profile, rate_matrix =
  let profile = SeqSim.random_profile ~alpha:1. rng in
  let root = SeqSim.draw_from_profile profile rng in
  let stationary_distribution = SeqSim.vec_of_profile profile in
  let exchangeabilities =
    Rate_matrix.Nucleotide.make_symetric (fun a b ->
        float ((a :> int) + 7 + (b :> int) + 7) /. 20.
      )
  in
  let rate_matrix = Rate_matrix.Nucleotide.gtr ~stationary_distribution ~exchangeabilities in
  let sim =
    NucSim.site_gillespie_direct rng small_tree ~root ~rate_matrix:(fun _ ->
        rate_matrix
      )
  in
  sim, profile, rate_matrix

let conditional_simulations =
  let map_tree =
    Tree.map
      ~leaf:Nucleotide.of_int_exn
      ~node:Nucleotide.of_int_exn
      ~branch:fst
  in
  let cl =
    Phylo_ctmc.conditional_likelihoods
      reference_simulation
      ~nstates:Nucleotide.card
      ~leaf_state:(fun (_, nuc) -> Nucleotide.to_int nuc)
      ~transition_probabilities:(fun bl ->
          let mat = Nucleotide.Matrix.(expm (scal_mul bl rate_matrix)) in
          (mat :> Linear_algebra.mat)
        )
  in
  let stationary_distribution = SeqSim.vec_of_profile profile in
  let root_frequencies = (stationary_distribution :> Linear_algebra.vec) in
  Array.init _B_ ~f:(fun _ ->
      Phylo_ctmc.conditional_simulation rng cl ~root_frequencies
      |> map_tree
    )

let rejection_sampling_simulations =
  let map_tree = Tree.map ~leaf:snd ~node:snd ~branch:Fun.id in
  let reference_leaves = Tree.leaves reference_simulation in
  let rec loop () =
    let root = SeqSim.draw_from_profile profile rng in
    let sim = NucSim.site_gillespie_direct rng small_tree ~root ~rate_matrix:(fun _ ->
        rate_matrix
      )
    in
    let leaves = Tree.leaves sim in
    let equal = List.equal (Tuple2.equal ~eq1:String.equal ~eq2:Nucleotide.equal) in
    if equal leaves reference_leaves then map_tree sim
    else loop ()
  in
  Array.init _B_ ~f:(fun _ -> loop ())

let nuc_counts xs =
  let create_table _ = Nucleotide.Table.init (Fun.const 0) in
  let incr table nuc = Nucleotide.Table.(set table nuc (get table nuc + 1)) in
  let rec iter2 acc t =
    match acc, t with
    | Tree.Leaf table, Tree.Leaf nuc -> incr table nuc
    | Node n_acc, Node n_t ->
      incr n_acc.data n_t.data ;
      List.iter2_exn
        (List1.to_list n_acc.branches)
        (List1.to_list n_t.branches)
        ~f:(fun (Branch b_acc) (Branch b_t) -> iter2 b_acc.tip b_t.tip)
    | _ -> assert false
  in
  let res =
    Tree.map reference_simulation ~node:create_table ~leaf:create_table ~branch:Fun.id
  in
  Array.iter xs ~f:(iter2 res) ;
  res

let all_counts =
  let pair x y = x, y in
  let first x _ = x in
  Tree.map2_exn
    (nuc_counts rejection_sampling_simulations)
    (nuc_counts conditional_simulations)
    ~leaf:pair ~node:pair ~branch:first

module B = PrintBox

let display_tree =
  let render_counts k =
    (k : int Nucleotide.table :> int array)
    |> Array.map ~f:(fun k -> B.sprintf "%.3f" (float k /. float _B_))
  in
  let render_node_info (k1, k2) =
    [| render_counts k1 ; render_counts k2 |]
    |> B.grid
    |> B.frame
  in
  let rec render = function
    | Tree.Leaf li -> render_node_info li
    | Node n ->
      B.tree
        (render_node_info n.data)
        (
          List1.to_list n.branches
          |> List.map ~f:(fun (Tree.Branch b) -> render b.tip)
        )
  in
  render all_counts

let () =
  print_endline {|
Tests if we obtain the same probability distribution for internal nodes of a tree
given observations on the leaves, whether we compute them using the pruning algorithm
or a rejection sampling algorithm.

|} ;
  PrintBox_text.output stdout display_tree
