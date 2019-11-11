val fancy_sprintf : ('a, unit, string, string, string, string) format6 -> 'a
val fancy_length : string -> int
val all_printers :
  ?options:(string -> string) list ->
  ('a -> string) ->
  (Format.formatter -> 'a -> unit) *
  (Format.formatter -> 'a -> unit) *
  ('a -> unit) * ('a -> unit)
val colorize : string -> string -> string -> string

type float_array = float array
[@@deriving show]

type float_array_array = float array array
[@@deriving show]

val robust_equal : float -> float -> bool
val float_array_robust_equal : float array -> float array -> bool

val random_profile : int -> Linear_algebra.vec
