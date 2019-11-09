open Core_kernel

module type T = sig
  val profile : int -> float array
end

module Owl_rng = struct
  let profile n =
    let v = Array.init n ~f:(fun _ ->
        Owl.Stats.uniform_rvs ~a:0. ~b:1.
      )
    in
    let s = Array.fold v ~init:0. ~f:( +. ) in
    Array.map v ~f:(fun x -> x /. s)
end
