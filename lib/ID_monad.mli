(** A state monad to generate integer identifiers *)

type 'a t

val return : 'a -> 'a t
val (let*) : 'a t -> ('a -> 'b t) -> 'b t
val (let+) : 'a t -> ('a -> 'b) -> 'b t

val new_id : int t

val run : 'a t -> 'a
