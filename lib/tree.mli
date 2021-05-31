type ('n, 'l, 'b) t =
  | Node of {
      data : 'n ;
      branches : ('n, 'l, 'b) branch List1.t ;
    }
  | Leaf of 'l

and ('n, 'l, 'b) branch = Branch of {
  data : 'b ;
  tip : ('n, 'l, 'b) t ;
}

val leaf : 'l -> (_, 'l, 'b) t

val node :
  'a ->
  ('a, 'b, 'c) branch List1.t ->
  ('a, 'b, 'c) t

val binary_node :
  'a ->
  ('a, 'b, 'c) branch ->
  ('a, 'b, 'c) branch ->
  ('a, 'b, 'c) t

val branch :
  'c ->
  ('a, 'b, 'c) t ->
  ('a, 'b, 'c) branch

val data : ('a, 'a, _) t -> 'a


val to_printbox :
  ?node:('n -> string) ->
  ?leaf:('l -> string) ->
  ?branch:('b -> string option) ->
  ('n, 'l, 'b) t ->
  PrintBox.t

val map :
  ('a, 'b, 'c) t ->
  node:('a -> 'd) ->
  leaf:('b -> 'e) ->
  branch:('c -> 'f) ->
  ('d, 'e, 'f) t

val propagate :
  ('n, 'l, 'b) t ->
  init:'d ->
  node:('d -> 'n -> 'd) ->
  leaf:('d -> 'l -> 'e) ->
  branch:('d -> 'b -> 'd) ->
  ('d, 'e, 'b) t

val prefix_traversal :
  ('n, 'l, 'b) t ->
  init:'c ->
  node:('c -> 'n -> 'c) ->
  leaf:('c -> 'l -> 'c) ->
  branch:('c -> 'b -> 'c) ->
  'c

val leaves : (_, 'l, _) t -> 'l list

val fold_leaves :
  (_, 'l, _) t ->
  init:'a ->
  f:('a -> 'l -> 'a) ->
  'a

val unfold :
  ('n, 'l, 'b) t ->
  init:'a ->
  branch:('e -> 'b -> 'a * 'bb) ->
  leaf:('a -> 'l -> 'e * 'll) ->
  node:('a -> 'n -> 'e * 'nn) ->
  ('nn, 'll, 'bb) t

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
