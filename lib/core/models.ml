open Sigs

module JC69 = struct
  type t = unit
  module Base = Nucleotide
  let transition () a b = if a=b then -1.0 else 1./.3.
  let stat_dis () _ = 0.25
  let has_decomposition = true
  let diag = function 1 -> 0.0 | _ -> -1.333333333333
  let diag_p i j =
    if j=1 then 1.0
    else if i=1 then -1.0
    else if i=j then 1.0
    else 0.0
  let diag_p_inv i j =
    if i=1 then 0.25
    else if i=j then 0.75
    else -0.25
end
