type ('n, 'l, 'b) t =
  | Node of {
      data : 'n ;
      branches : ('n, 'l, 'b) branch Non_empty_list.t ;
    }
  | Leaf of 'l

and ('n, 'l, 'b) branch = Branch of {
  data : 'b ;
  tip : ('n, 'l, 'b) t ;
}

val leaf : 'l -> (_, 'l, 'b) t

val node :
  'a ->
  ('a, 'b, 'c) branch Non_empty_list.t ->
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

val pre :
  ('n, 'l, 'b) t ->
  init:'c ->
  node:('c -> 'n -> 'c) ->
  leaf:('c -> 'l -> 'c) ->
  branch:('c -> 'b -> 'c) ->
  'c

val leaves : (_, 'l, _) t -> 'l list

val map_leaves :
  ('a, 'l, 'b) t ->
  root:'b ->
  f:('l -> 'b -> 'c) ->
  'c list

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
