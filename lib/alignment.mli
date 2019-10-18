(** Module for alignments (sets of aligned sequences with same length).
    Provides a functor to build from SEQUENCE modules. *)

open Sigs

(** Main functor ; uses a hashtable. *)
module Make (S : Seq.S):
  ALIGNMENT with type base = S.base and type sequence = S.t
