(** Compilation of modules implementing evolution models and
    providing relevant mathematical procedure (eg, exponential of transition matrix);
    also includes functors to build models from transition matrices.*)

open Sigs

(** Jukes-Cantor model with analytical diagonalization of transition matrix. *)
module JC69: EVOL_MODEL with type t = unit

(** Jukes-Cantor with numerical diagonalization of transition matrix *)
module JC69_generated: EVOL_MODEL with type t = unit

(** K80 model with analytical diagonalization of transition matrix (parametrized by kappa) *)
module K80: EVOL_MODEL with type t = float

(** K80 model with numerical diagonalization of transition matrix
    (needs to recompute for every value of kappa) *)
module K80_generated: EVOL_MODEL with type t = float

(** Model + parameter bundle *)
type t = {model:(module EVOL_MODEL) ; param:string}

(** Returns a model + parameter from a string specifying the model (bpp format).
    Eg, "K80(kappa=2.0)" returns module K80 and parameter 2.0.*)
val of_string: string -> t
