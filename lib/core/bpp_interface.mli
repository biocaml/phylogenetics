(** Runs bppml and extracts initial log likelihood. Needs bppml in PATH. *)
val felsenstein_bpp: ?alphabet:string -> ?model:string -> ?path:string -> tree:string -> string -> float

val seqgen_bpp: ?alphabet:string -> ?model:string -> ?path:string -> tree:string -> string -> int -> unit
