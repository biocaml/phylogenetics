open Core
open Phylogenetics

let rng = Gsl.Rng.(make (default ()))
let wag = Wag.from_file_exn "../data/wag.dat"
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

module AASim = Phylogenetics.Simulator.Make(Amino_acid)(Branch_info)
module SeqSim = Sequence_simulator.Make(Amino_acid)

let reference_simulation, profile, rate_matrix =
  let profile = SeqSim.random_profile ~alpha:1. rng in
  let root = SeqSim.draw_from_profile profile rng in
  let stationary_distribution = SeqSim.vec_of_profile profile in
  let exchangeabilities = wag.rate_matrix in
  let rate_matrix = Rate_matrix.Amino_acid.gtr ~stationary_distribution ~exchangeabilities in
  let sim =
    AASim.site_gillespie_direct rng small_tree ~root ~rate_matrix:(fun _ ->
        rate_matrix
      )
  in
  sim, profile, rate_matrix

let conditional_simulations =
  let map_tree =
    Tree.map
      ~leaf:Amino_acid.of_int_exn
      ~node:Amino_acid.of_int_exn
      ~branch:fst
  in
  let cl =
    Phylo_ctmc.conditional_likelihoods
      reference_simulation
      ~nstates:Amino_acid.card
      ~leaf_state:(fun (_, aa) -> Amino_acid.to_int aa)
      ~transition_probabilities:(fun bl ->
          let mat = Amino_acid.Matrix.(expm (scal_mul bl rate_matrix)) in
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
    let sim = AASim.site_gillespie_direct rng small_tree ~root ~rate_matrix:(fun _ ->
        rate_matrix
      )
    in
    let leaves = Tree.leaves sim in
    let equal = List.equal (Tuple2.equal ~eq1:String.equal ~eq2:Amino_acid.equal) in
    if equal leaves reference_leaves then map_tree sim
    else loop ()
  in
  Array.init _B_ ~f:(fun _ -> loop ())

let aa_counts xs =
  let create_table _ = Amino_acid.Table.init (Fun.const 0) in
  let incr table aa = Amino_acid.Table.(set table aa (get table aa + 1)) in
  let rec iter2 acc t =
    match acc, t with
    | Tree.Leaf table, Tree.Leaf aa -> incr table aa
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
    (aa_counts rejection_sampling_simulations)
    (aa_counts conditional_simulations)
    ~leaf:pair ~node:pair ~branch:first

module B = PrintBox

let display_tree =
  let render_counts k =
    (k : int Amino_acid.table :> int array)
    |> Array.map ~f:(fun k -> B.sprintf "%.2f" (float k /. float _B_))
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

let () = PrintBox_text.output stdout display_tree
