(** Phylogenetic inference by parcimony

    The Fitch algorithm is a method used in phylogenetics to infer
    ancestral states of characters (e.g., genetic traits) on a given
    evolutionary tree. It assigns category indices to the nodes of the
    tree based on the observed states at the leaf nodes and the
    inferred states at the internal nodes.

    The algorithm has two steps:
    - {!forward} which performs a first pass an input tree, computing
      annotating each node with the minimal cost for the corresponding
      subtree for each possible choice of the node category
    - {!backward} which uses costs computed at the {!forward} pass to
      select the category with minimal cost
*)

val forward :
  ?cost:(int -> int -> float) ->
  n:int ->
  category:('l -> int option) ->
  ('n, 'l, 'b) Tree.t ->
  float array * ('n * int List1.t array, 'l, 'b) Tree.t
(** [forward ?cost ~n ~category t] performs the forward pass of the
    Fitch algorithm on a given tree.

   - [?cost] is an optional cost function that takes two integers
     representing categories and returns a float representing the cost
     between them. If not provided, a default cost function is used
     which returns 1 if its arguments are different according to the
     polymorphic equality
   - [n] is an integer representing the number of categories.
   - [category] is a function that takes a leaf label and returns an
     optional category index for that label.
   - [t] is the input tree.

   It returns a pair made of:
   - an array of floats representing the accumulated costs for each
     category.
   - a tree where each node is annotated with a pair consisting of the
     original node data and an array of choices (category indices) for
     each branch.

   Example:
    {[
      let category_func leaf_data =
        match leaf_data with
        | "A" -> Some 1
        | "B" -> Some 2
        | _ -> None

      let input_tree = Tree.(node 1 [branch 1 (leaf "A"); branch 2 (leaf "B")])

      let costs, routing = forward ~n:2 ~category:category_func input_tree

      (* Accessing the costs *)
      let cost_a_b = costs.(1).(2) (* Cost between category 1 and 2 *)

      (* Accessing the routing *)
      let node_data, children = routing.data
      let child_1, child_2 = children.(1), children.(2) (* Children of the root node *)
    ]}

    This example demonstrates how to use the [forward] function to
    compute the costs and routing of a tree. We define a
    [category_func] that assigns categories to leaf data, where "A"
    corresponds to category 1 and "B" corresponds to category 2. We
    then create an input tree with two branches: one for category 1
    and one for category 2. By invoking [forward] with the appropriate
    arguments, we obtain the computed costs in the [costs] array and
    the resulting routing in the [routing] tree. We can access the
    individual costs using array indexing, and we can access the
    routing information by navigati g the [routing] tree structure.
*)

val backward :
  float array ->
  ('n * int List1.t array, 'l, 'b) Tree.t ->
  ('n * int, 'l * int, 'b) Tree.t
(** [backward costs tree] performs the backward pass of the Fitch
    algorithm on a given tree.

    The arguments [costs] and [tree] are the values returned by the
    {!forward} function.

   It returns a tree where each branch is annotated with a pair
   consisting of the original branch data and an integer representing
   the chosen category index.

   Example:
    {[
      let input_tree = Tree.(node (1, [|List1.[1; 2]|]) [branch (1, [|List1.[1]|]) (leaf ("A", 1)); branch (2, [|List1.[2]|]) (leaf ("B", 2))])
      let costs = [| [|0.; 2.|]; [|2.; 0.|] |]

      let optimal_routing = backward costs input_tree

      (* Accessing the routing *)
      let node_data, children = optimal_routing.data
      let child_1, child_2 = children.(1), children.(2) (* Children of the root node *)
      let category_1, path_1 = child_1.tip.data (* Category and path of child 1 *)
      let category_2, path_2 = child_2.tip.data (* Category and path of child 2 *)
    ]}

    This example showcases the usage of the [backward] function to
    compute the optimal routing of a tree. We provide a [costs] array
    representing the costs between different categories. The
    [input_tree] represents a pre-defined tree structure. By calling
    [backward] with the appropriate arguments, we obtain the
    [optimal_routing] tree, which contains the computed optimal
    routing information. We can access the routing information by
    traversing the [optimal_routing] tree structure and retrieving the
    category and path for each child node.

*)

val fitch :
  ?cost:(int -> int -> float) ->
  n:int ->
  category:('l -> int option) ->
  ('n, 'l, 'b) Tree.t ->
  ('n * int, 'l * int, 'b) Tree.t
(** [fitch ?cost ~n ~category tree] performs the Fitch algorithm on a
    given tree.

   - [?cost] is an optional cost function that takes two integers
     representing categories and returns a float representing the cost
     between them. If not provided, a default cost function is used
     which returns 1 if its arguments are different according to the
     polymorphic equality
   - [n] is an integer representing the number of categories.
   - [category] is a function that takes a leaf label and returns an
     optional category index for that label.
   - [t] is the input tree.

   It returns a tree where each branch is annotated with a pair
   consisting of the original branch data and an integer representing
   the chosen category index.

    Example:

    {[
      let category_func leaf_data =
        match leaf_data with
        | "A" -> Some 1
        | "B" -> Some 2
        | _ -> None

      let input_tree = Tree.(node 1 [branch 1 (leaf "A"); branch 2 (leaf "B")])

      let optimal_routing = fitch ~n:2 ~category:category_func input_tree

      (* Accessing the routing *)
      let node_data, children = optimal_routing.data
      let child_1, child_2 = children.(1), children.(2) (* Children of the root node *)
      let category_1, path_1 = child_1.tip.data (* Category and path of child 1 *)
      let category_2, path_2 = child_2.tip.data (* Category and path of child 2 *)
    ]}

   This example demonstrates how to use the [fitch] function to
   compute the optimal routing of a tree. We define a [category_func]
   that assigns categories to leaf data, where "A" corresponds to
   category 1 and "B" corresponds to category 2. We then create an
   input tree with two branches: one for category 1 and one for
   category 2. By invoking [fitch] with the appropriate arguments, we
   obtain the computed optimal routing in the [optimal_routing]
   tree. We can access the routing information by navigating the
   [optimal_routing] tree structure and retrieving the category and
   path for each child node.
*)
