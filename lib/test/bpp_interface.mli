(** Runs bppml and extracts initial log likelihood. Needs bppml in PATH. *)
val felsenstein_bpp: ?model:string -> ?path:string -> tree:string -> string -> float
