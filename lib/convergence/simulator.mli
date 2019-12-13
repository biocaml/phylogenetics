open Phylogenetics

type tree = (Newick_ast.node_info, Newick_ast.node_info, float * int) Tree.t

val root_condition : (_, _, (_ * 'c)) Tree.t -> 'c option

val pair_tree :
  branch_length1:float ->
  branch_length2:float ->
  npairs:int ->
  Newick.tree

module Mutsel : sig
  include module type of Phylogenetics.Simulator.Mutsel

  val condition_dependent_alignment :
    fitness_profiles:float array array ->
    tree:tree ->
    int ->
    Dna.t list
end
