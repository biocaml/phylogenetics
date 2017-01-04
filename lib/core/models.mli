open Sigs

module JC69: EVOL_MODEL with type t = unit

module K80: EVOL_MODEL with type t = float

type model = {model:(module EVOL_MODEL) ; param:string}

val of_string: string -> model
