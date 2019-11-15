open Core_kernel

type t =
  | JC69
  | K80 of float

let to_string = function
  | JC69 -> "JC69"
  | K80 kappa -> sprintf "K80(kappa=%f)" kappa
