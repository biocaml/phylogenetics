open Phylogenetics

type condition = int
type tree = (unit, int * condition, float * condition) Tree.t

val newick_tree_of_tree : tree -> Newick.tree

val root_condition : (_, _, (_ * 'c)) Tree.t -> 'c option

val pair_tree :
  branch_length1:float ->
  branch_length2:float ->
  npairs:int ->
  tree

module Mutsel : sig
  include module type of Phylogenetics.Simulator.Mutsel

  val condition_dependent_alignment :
    fitness_profiles:float array array ->
    tree:tree ->
    int ->
    Dna.t list
end
