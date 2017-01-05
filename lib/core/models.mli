open Sigs

module JC69: EVOL_MODEL with type t = unit
module JC69_generated: EVOL_MODEL with type t = unit

module K80: EVOL_MODEL with type t = float
module K80_generated: EVOL_MODEL with type t = float

type t = {model:(module EVOL_MODEL) ; param:string}

val of_string: string -> t

module Make (M:TRANSITION_MATRIX):EVOL_MODEL
