type 'a t = Cons of 'a * 'a list
[@@deriving sexp]

val hd : 'a t -> 'a
val length : _ t -> int
val singleton : 'a -> 'a t
val cons : 'a -> 'a list -> 'a t
val cons1 : 'a -> 'a t -> 'a t
val init : int -> f:(int -> 'a) -> 'a t
val rev : 'a t -> 'a t
val map : 'a t -> f:('a -> 'b) -> 'b t
val mapi : 'a t -> f:(int -> 'a -> 'b) -> 'b t
val map2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> ('c t, [> `Unequal_lengths]) result
val map2_exn : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
val filter : 'a t -> f:('a -> bool) -> 'a list
val filter_map : 'a t -> f:('a -> 'b option) -> 'b list
val iter : 'a t -> f:('a -> unit) -> unit
val fold : 'a t -> init:'c -> f:('c -> 'a -> 'c) -> 'c
val fold_right : 'a t -> init:'c -> f:('a -> 'c -> 'c) -> 'c
val reduce : 'a t -> f:('a -> 'a -> 'a) -> 'a
val to_list : 'a t -> 'a list
val of_list : 'a list -> 'a t option
val of_list_exn : 'a list -> 'a t
val unzip : ('a * 'b) t -> 'a t * 'b t
val for_all : 'a t -> f:('a -> bool) -> bool
val exists : 'a t -> f:('a -> bool) -> bool
val sort : 'a t -> compare:('a -> 'a -> int) -> 'a t
