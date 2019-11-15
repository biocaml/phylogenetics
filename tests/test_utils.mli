(** Various utility functions to be used in tests.
    Includes bio++ interfaces and Alcotest comparison functions. *)

open Phylogenetics


(** {6 Comparison functions (including Alcotest testables)} *)

(** Compares two distributions using estimators TODO *)
val check_distrib: Stat_tools.sample_list -> Stat_tools.sample_list -> unit

(** Compares two floats (which are supposed to be likelihood results) using the alcotest check *)
val check_likelihood: float -> float -> unit

val compare_matrices:
  (module Alphabet.S with type matrix = 'mat) ->
  string ->
  'mat ->
  'mat ->
  unit


(** {6 Interfaces for external runs of bppml.}
    Uses Sys and needs bpp executables in PATH.*)

(** Runs bppml and extracts initial log likelihood. Needs bppml in PATH. *)
val felsenstein_bpp: ?alphabet:string -> ?model:string -> ?path:string -> tree:string -> string -> float

(** Runs bppseqgen and writes the result in a fasta file. Needs bppseqgen in PATH. *)
val seqgen_bpp: ?alphabet:string -> ?model:string -> ?path:string -> tree:string -> string -> int -> unit
