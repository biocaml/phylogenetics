type 'a sequence = (int * 'a) list

val get_base: int -> 'a sequence -> 'a

val test_get_base: unit -> string
