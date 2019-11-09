type t = private string

val of_string_unsafe : string -> t
val of_string_exn : string -> t
val gc_contents : t -> float
