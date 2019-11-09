type 'a t = Cons of 'a * 'a list
val cons : 'a -> 'a list -> 'a t
val init : int -> f:(int -> 'a) -> 'a t
val map : 'a t -> f:('a -> 'b) -> 'b t
val iter : 'a t -> f:('a -> unit) -> unit
val fold : 'a t -> init:'c -> f:('c -> 'a -> 'c) -> 'c
val to_list : 'a t -> 'a list
