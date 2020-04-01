type 'a t = Cons of 'a * 'a list
val hd : 'a t -> 'a
val singleton : 'a -> 'a t
val cons : 'a -> 'a list -> 'a t
val init : int -> f:(int -> 'a) -> 'a t
val map : 'a t -> f:('a -> 'b) -> 'b t
val map2_exn : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
val filter_map : 'a t -> f:('a -> 'b option) -> 'b t option
val iter : 'a t -> f:('a -> unit) -> unit
val fold : 'a t -> init:'c -> f:('c -> 'a -> 'c) -> 'c
val to_list : 'a t -> 'a list
val of_list_exn : 'a list -> 'a t
val unzip : ('a * 'b) t -> 'a t * 'b t
val for_all : 'a t -> f:('a -> bool) -> bool
val exists : 'a t -> f:('a -> bool) -> bool
val sort : 'a t -> compare:('a -> 'a -> int) -> 'a t
