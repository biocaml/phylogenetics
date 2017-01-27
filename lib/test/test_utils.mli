(** Interfaces for external runs of bppml. Uses Sys and needs bpp executables in PATH. *)

(** Runs bppml and extracts initial log likelihood. Needs bppml in PATH. *)
val felsenstein_bpp: ?alphabet:string -> ?model:string -> ?path:string -> tree:string -> string -> float

(** Runs bppseqgen and writes the result in a fasta file. Needs bppseqgen in PATH. *)
val seqgen_bpp: ?alphabet:string -> ?model:string -> ?path:string -> tree:string -> string -> int -> unit
