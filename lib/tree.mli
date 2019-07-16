type ('a, 'b) t = {
  node_data : 'a ;
  branches : ('a, 'b) branch list ;
}

and ('a, 'b) branch = {
  branch_data : 'b ;
  tip : ('a, 'b) t ;
}

val map :
  ('a, 'b) t ->
  node:('a -> 'c) ->
  branch:('b -> 'd) ->
  ('c, 'd) t

val propagate :
  ('a, 'b) t ->
  root:'c ->
  node:('c -> 'b -> 'c) ->
  branch:('c -> 'b -> 'd) ->
  ('c, 'd) t

val pre :
  ('a, 'b) t ->
  init:'c ->
  node:('c -> ('a, 'b) t -> 'c) ->
  branch:('c -> ('a, 'b) branch -> 'c) ->
  'c

val leaves : ('a, 'b) t -> 'a list

val map_leaves :
  ('a, 'b) t ->
  root:'b ->
  f:('a -> 'b -> 'c) ->
  'c list

val node_prefix_synthesis :
  ('a, 'b) t ->
  init:'c ->
  f:('c -> 'a -> 'd list -> 'c * 'd) ->
  ('d, 'b) t
