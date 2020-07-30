open Phylogenetics
open Core_kernel

type condition = int
type tree = (unit, int * condition, float * condition) Tree.t

let root_condition (tree : (_, _, (_ * 'c)) Tree.t) =
  let module C = Biocaml_unix.Accu.Counter in
  let acc = C.create () in
  (
    match tree with
    | Leaf _ -> ()
    | Node n ->
      List1.iter n.branches ~f:(fun (Branch b) -> C.tick acc (snd b.data))
  ) ;
  match
    C.to_alist acc
    |> List.sort ~compare:(fun (_, k) (_, l) -> Int.compare l k)
  with
  | [] -> None
  | [ h, _ ] -> Some h
  | (h1, k1) :: (_, k2) :: _ ->
    if k1 > k2 then Some h1
    else None

module Mutsel = struct
  include Simulator.Make(Mutsel.NSCodon)(Mutsel)

  let alignment tree ~root param =
    List.init (Array.length root) ~f:(fun i ->
        site_gillespie_direct tree ~root:root.(i) ~param:(param i)
        |> Tree.leaves
        |> List.map ~f:Codon.Universal_genetic_code.NS.to_string
      )
    |> List.transpose_exn
    |> List.map ~f:String.concat
    |> List.map ~f:Dna.of_string_unsafe

  let nb_conditions t =
    Tree.pre t
      ~init:Int.Set.empty
      ~node:Fn.const
      ~leaf:Fn.const
      ~branch:(fun acc (_, i) -> Int.Set.add acc i)
    |> Int.Set.length

  let condition_dependent_alignment ~fitness_profiles ~(tree : tree) n =
    let nb_conditions = nb_conditions tree in
    let p = Mutsel.random_param ~alpha_nucleotide:10. ~alpha_fitness:10. in
    let profile_assignment =
      Array.init n ~f:(fun _ ->
          Array.init nb_conditions ~f:(fun _ ->
              Random.int (Array.length fitness_profiles)
            )
        )
    in
    let param pos condition =
      let scaled_fitness =
        fitness_profiles.(profile_assignment.(pos).(condition))
        |> Amino_acid.Vector.of_array_exn
      in
      { p with scaled_fitness }
    in
    let root_condition =
      match root_condition tree with
      | None -> failwith "No most parcimonious root condition"
      | Some c -> c
    in
    let root_params = Array.init n ~f:(fun i -> param i root_condition) in
    let root_stationary_distributions = Array.map root_params ~f:(fun p ->
        Mutsel.stationary_distribution p
        |> Mutsel.NSCodon.Table.of_vector
      )
    in
    let root = hmm0 ~len:n ~dist:(Array.get root_stationary_distributions) in
    alignment tree ~root param

end

let pair_tree ~branch_length1 ~branch_length2 ~npairs : tree =
  let leaf i cond = Tree.Leaf (i, cond) in
  let tree = Tree.binary_node () in
  let make_pair i =
    tree
      (Tree.branch (branch_length2, 0) (leaf i 0))
      (Tree.branch (branch_length2, 1)  (leaf i 1))
    |> Tree.branch (branch_length1, 0)
  in
  Tree.node ()  (List1.init npairs ~f:make_pair)

let newick_tree_of_tree (tree : tree) =
  Tree.map tree
    ~leaf:(fun (i, cond) ->
        let name = Some (sprintf "Spe-%d-%d" cond i) in
        { Newick_ast.name }
      )
    ~node:(Fn.const { Newick_ast.name = None })
    ~branch:(fun (length, condition) ->
        let tags = ["Condition", Int.to_string condition] in
        { Newick_ast.length = Some length ; tags }
      )
