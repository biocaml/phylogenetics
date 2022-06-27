include Alphabet.S_int

val yojson_of_vector : vector -> Yojson.Safe.t
val vector_of_yojson : Yojson.Safe.t -> vector

val to_char : t -> char
val of_char : char -> t option
val of_char_exn : char -> t