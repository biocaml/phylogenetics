open Phylogenetics

type condition = [`Ancestral | `Convergent]

module Branch_info : sig
  type t = {
    length : float ;
    condition : condition ;
  }
  val length : t -> float
  val condition : t -> condition
end

type leaf_id = int
type tree = (unit, leaf_id * condition, Branch_info.t) Tree.t

val newick_tree_of_tree : tree -> Newick.tree


val pair_tree :
  branch_length1:float ->
  branch_length2:float ->
  npairs:int ->
  tree

module Mutsel : sig
  include module type of Simulator.Mutsel(Branch_info)

  val condition_dependent_alignment :
    fitness_profiles:float array array ->
    tree:tree ->
    int ->
    Dna.t list
end

module More_than_two_conditions : sig
  type condition = int
  val root_condition : (_, _, (_ * condition)) Tree.t -> condition option
  val nb_conditions : (_, _, (_ * condition)) Tree.t -> condition
end
