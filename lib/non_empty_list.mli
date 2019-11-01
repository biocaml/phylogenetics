type 'a t = Cons of 'a * 'a list
val cons : 'a -> 'a list -> 'a t
val map : 'a t -> f:('a -> 'b) -> 'b t
val fold : 'a t -> init:'c -> f:('c -> 'a -> 'c) -> 'c
val to_list : 'a t -> 'a list
