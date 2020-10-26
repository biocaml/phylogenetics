open Phylogenetics
open Core_kernel

type condition = [`Ancestral | `Convergent]
let int_of_condition = function
  | `Ancestral -> 0
  | `Convergent -> 1

module Branch_info = struct
  type t = {
    length : float ;
    condition : condition ;
  }
  let length b = b.length
  let condition b = b.condition
end

type leaf_id = int
type tree = (unit, leaf_id * condition, Branch_info.t) Tree.t

module Mutsel = struct
  include Simulator.Make(Mutsel.NSCodon)(Mutsel)(Branch_info)

  let alignment tree ~root param =
    List.init (Array.length root) ~f:(fun i ->
        site_gillespie_direct tree ~root:root.(i) ~param:(param i)
        |> Tree.leaves
        |> List.map ~f:Codon.Universal_genetic_code.NS.to_string
      )
    |> List.transpose_exn
    |> List.map ~f:String.concat
    |> List.map ~f:Dna.of_string_unsafe

  let condition_dependent_alignment ~fitness_profiles ~(tree : tree) n =
    let nb_conditions = 2 (* nb_conditions tree *) in
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
        fitness_profiles.(profile_assignment.(pos).(int_of_condition condition))
        |> Amino_acid.Vector.of_array_exn
      in
      { p with scaled_fitness }
    in
    let root_condition = `Ancestral in
    let root_params = Array.init n ~f:(fun i -> param i root_condition) in
    let root_stationary_distributions = Array.map root_params ~f:(fun p ->
        Mutsel.stationary_distribution p
        |> Mutsel.NSCodon.Table.of_vector
      )
    in
    let root = hmm0 ~len:n ~dist:(Array.get root_stationary_distributions) in
    alignment tree ~root (fun pos (bi : Branch_info.t) -> param pos bi.condition)

end

let pair_tree ~branch_length1 ~branch_length2 ~npairs : tree =
  let leaf i cond = Tree.Leaf (i, cond) in
  let branch length condition tip = Tree.branch { Branch_info.length ; condition } tip in
  let tree = Tree.binary_node () in
  let make_pair i =
    tree
      (branch branch_length2 `Ancestral (leaf i `Ancestral))
      (branch branch_length2 `Convergent (leaf i `Convergent))
    |> branch branch_length1 `Ancestral
  in
  Tree.node ()  (List1.init npairs ~f:make_pair)

let newick_tree_of_tree (tree : tree) =
  Tree.map tree
    ~leaf:(fun (i, cond) ->
        let name = Some (sprintf "Spe-%d-%d" (int_of_condition cond) i) in
        { Newick_ast.name }
      )
    ~node:(Fn.const { Newick_ast.name = None })
    ~branch:(fun { Branch_info.length ; condition } ->
        let tags = ["Condition", Int.to_string (int_of_condition condition)] in
        { Newick_ast.length = Some length ; tags }
      )

module More_than_two_conditions = struct
  type condition = int
  let nb_conditions t =
    Tree.pre t
      ~init:Int.Set.empty
      ~node:Fn.const
      ~leaf:Fn.const
      ~branch:(fun acc (_, cond) -> Int.Set.add acc cond)
    |> Int.Set.length

  let root_condition (tree : (_, _, _) Tree.t) =
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
end
