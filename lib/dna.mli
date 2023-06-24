(** Strings that represent valid DNA sequences *)

type t = private string

val of_string_unsafe : string -> t
val of_string_exn : string -> t
val of_codons : Codon.Universal_genetic_code.NS.t array -> t
val gc_contents : t -> float
