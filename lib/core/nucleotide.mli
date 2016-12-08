open Sigs

(** A single DNA base *)
type t = A | C | G | T

include BASE with type t := t
