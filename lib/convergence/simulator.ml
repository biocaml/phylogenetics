open Phylogenetics
open Core_kernel

type tree = (Newick_ast.node_info, Newick_ast.node_info, float * int) Tree.t

let root_condition (tree : (_, _, (_ * 'c)) Tree.t) =
  let module C = Biocaml_unix.Accu.Counter in
  let acc = C.create () in
  (
    match tree with
    | Leaf _ -> ()
    | Node n ->
      Non_empty_list.iter n.branches ~f:(fun (Branch b) -> C.tick acc (snd b.data))
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

  let condition_dependent_alignment ~fitness_profiles ~tree n =
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

let pair_tree ~branch_length1 ~branch_length2 ~npairs : _ Tree.t =
  let leaf ?name () = Tree.Leaf { Newick_ast.name } in
  let tree ?name = Tree.binary_node { Newick_ast.name } in
  let branch ~length ~condition tip =
    let tags = match condition with
      | `Ancestral -> ["Condition", "0"]
      | `Convergent -> ["Condition", "1" ; "Transition", "1"]
    in
    Tree.Branch {
      data = { Newick_ast.length = Some length ; tags } ;
      tip ;
    }
  in
  let make_pair i =
    tree
      (branch ~length:branch_length2 ~condition:`Ancestral (leaf ~name:(sprintf "A%d" i) ())) 
      (branch ~length:branch_length2 ~condition:`Convergent (leaf ~name:(sprintf "C%d" i) ()))
    |> branch ~length:branch_length1 ~condition:`Ancestral
  in
  Tree.node { Newick_ast.name = None }  (Non_empty_list.init npairs ~f:make_pair)
