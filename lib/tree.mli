(** Tree structure with annotations on leaves, internal nodes and
    branches. *)

type ('n, 'l, 'b) t =
  | Node of {
      data : 'n ;
      branches : ('n, 'l, 'b) branch List1.t ;
    }
  | Leaf of 'l
[@@deriving sexp]
(** [Node { data; branches }] represents a node in the tree with node
    data of type ['n] and a non-empty list of branches

    [Leaf l] represents a leaf in the tree with leaf data of type
    ['l].
*)

and ('n, 'l, 'b) branch = Branch of {
    data : 'b ;
    tip : ('n, 'l, 'b) t ;
  }

val leaf : 'l -> (_, 'l, 'b) t
(** [leaf l] constructs a leaf node with data [l]. *)

val node :
  'a ->
  ('a, 'b, 'c) branch List1.t ->
  ('a, 'b, 'c) t
(** [node a branches] constructs a node with data [a] and the given
    list of branches. *)

val binary_node :
  'a ->
  ('a, 'b, 'c) branch ->
  ('a, 'b, 'c) branch ->
  ('a, 'b, 'c) t
(** [binary_node a b1 b2] constructs a binary node with data [a] and
    the two branches [b1] and [b2]. *)

val branch :
  'c ->
  ('a, 'b, 'c) t ->
  ('a, 'b, 'c) branch
(** [branch c tip] constructs a branch with data [c] and the given
    sub-tree [tip]. *)

val data : ('a, 'a, _) t -> 'a
(** [data t] returns the data contained in the leaf or root node of
    the tree [t]. *)

val to_printbox :
  ?node:('n -> string) ->
  ?leaf:('l -> string) ->
  ?branch:('b -> string option) ->
  ('n, 'l, 'b) t ->
  PrintBox.t
(** [to_printbox ?node ?leaf ?branch t] converts the tree [t] to a
    printable representation using the provided functions for
    converting node, leaf, and branch data to strings. *)

val map :
  ('a, 'b, 'c) t ->
  node:('a -> 'd) ->
  leaf:('b -> 'e) ->
  branch:('c -> 'f) ->
  ('d, 'e, 'f) t
(** [map t ~node ~leaf ~branch] maps the node, leaf, and branch data
    of the tree [t] using the provided functions [node], [leaf], and
    [branch], respectively. *)

val map_branches :
  ('a, 'b, 'c) t ->
  node:('a -> 'd) ->
  leaf:('b -> 'd) ->
  branch:('d -> 'c -> 'd -> 'e) ->
  ('a, 'b, 'e) t

val map2_exn :
  ('a, 'b, 'c) t ->
  ('d, 'e, 'f) t ->
  node:('a -> 'd -> 'x) ->
  leaf:('b -> 'e -> 'y) ->
  branch:('c -> 'f -> 'z) ->
  ('x, 'y, 'z) t
(** [map2_exn t1 t2 ~node ~leaf ~branch] maps the corresponding node,
    leaf, and branch data of the trees [t1] and [t2] using functions
    [node], [leaf], and [branch], respectively. Raises an exception if
    the two trees have different structures. *)

val map_branch2_exn :
  ('a, 'b, 'c) branch ->
  ('d, 'e, 'f) branch ->
  node:('a -> 'd -> 'x) ->
  leaf:('b -> 'e -> 'y) ->
  branch:('c -> 'f -> 'z) ->
  ('x, 'y, 'z) branch
(** [map_branch2_exn b1 b2 ~node ~leaf ~branch] maps the corresponding
    node, leaf, and branch data of the branches [b1] and [b2] using
    functions [node], [leaf], and [branch], respectively. Raises an
    exception if the two branches have different structures. *)

val propagate :
  ('n1, 'l1, 'b1) t ->
  init:'s ->
  node:('s -> 'n1 -> 's * 'n2) ->
  leaf:('s -> 'l1 -> 'l2) ->
  branch:('s -> 'b1 -> 's * 'b2) ->
  ('n2, 'l2, 'b2) t
(** [propagate t ~init ~node ~leaf ~branch] propagates the values of
    node, leaf, and branch data in the tree [t] using the provided
    update functions [node], [leaf], and [branch]. The initial state
    [init] is threaded through the propagation. *)

val prefix_traversal :
  ('n, 'l, 'b) t ->
  init:'c ->
  node:('c -> 'n -> 'c) ->
  leaf:('c -> 'l -> 'c) ->
  branch:('c -> 'b -> 'c) ->
  'c
(** [prefix_traversal t ~init ~node ~leaf ~branch] performs a prefix
    traversal of the tree [t] using the provided update functions
    [node], [leaf], and [branch]. The initial state [init] is threaded
    through the traversal. Returns the final state after the
    traversal. *)

val leaves : (_, 'l, _) t -> 'l list
(** [leaves t] returns a list of all leaf data in the tree [t]. *)

val fold_leaves :
  (_, 'l, _) t ->
  init:'a ->
  f:('a -> 'l -> 'a) ->
  'a
(** [fold_leaves t ~init ~f] folds over the leaf data in the tree [t]
    using the provided folding function [f] and initial accumulator
    [init]. Returns the final accumulator after folding. *)

val unfold :
  ('n, 'l, 'b) t ->
  init:'a ->
  branch:('e -> 'b -> 'a * 'bb) ->
  leaf:('a -> 'l -> 'e * 'll) ->
  node:('a -> 'n -> 'e * 'nn) ->
  ('nn, 'll, 'bb) t
(** [unfold t ~init ~branch ~leaf ~node] unfolds the tree [t] into a
    new tree using the provided functions [branch], [leaf], and
    [node]. The initial state [init] is threaded through the
    unfolding. *)

(* val node_prefix_synthesis :
 *   ('a, 'b, 'l) t ->
 *   init:'c ->
 *   f:('c -> 'a -> 'd list -> 'c * 'd) ->
 *   ('d, 'b) t *)

val leafset_generated_subtree :
  ('n, 'l, 'b) t ->
  ('l -> string option) ->
  string list ->
  ('n, 'l, 'b) t option
(** [leafset_generated_subtree t f xs] returns the maximal subtree of
    [t] whose leaves all return a string in [xs] when applied to [f] if
    it exists (and [None] otherwise) *)

val simplify_node_with_single_child :
  merge_branch_data:('b list -> 'b) ->
  ('n, 'l, 'b) t ->
  ('n, 'l, 'b) t
(** [simplify_node_with_single_child ~merge_branch_data t] simplifies
    the tree [t] by merging nodes with a single child using the
    provided merging function [merge_branch_data]. *)
