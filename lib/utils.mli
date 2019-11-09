val fancy_sprintf : ('a, unit, string, string, string, string) format6 -> 'a
val fancy_length : string -> int
val all_printers :
  ?options:(string -> string) list ->
  ('a -> string) ->
  (Format.formatter -> 'a -> unit) *
  (Format.formatter -> 'a -> unit) *
  ('a -> unit) * ('a -> unit)
val colorize : string -> string -> string -> string
