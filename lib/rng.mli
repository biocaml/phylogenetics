module type T = sig
  val profile : int -> float array
end

module Owl_rng : T
