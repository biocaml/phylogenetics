(** Module for sequences of bases. Provides a functor to build the module
    for any base module with the BASE signature.*)

open Sigs

(** Main functor (using arrays) *)
module Make (B:BASE): SEQUENCE with type base = B.t

(** List-based constructor (no particular reason to use it) *)
module Make_list (B:BASE): SEQUENCE with type base = B.t

(** Pre-built DNA module. *)
module DNA: module type of Make(Nucleotide)
