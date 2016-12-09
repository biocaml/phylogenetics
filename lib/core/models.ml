open Sigs

module JC69 = struct
  type t = unit
  module Base = Nucleotide
  let transition () a b = if a=b then -1.0 else 1./.3.
  let stat_dis () a = 0.25
end
