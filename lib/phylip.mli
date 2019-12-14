(** The original format is described
   {{:http://evolution.genetics.washington.edu/phylip.html}there}. This
   implementation assumes a somewhat more relaxed syntax:
    {v
<nb sequences> <nb cols>
<id> TAB <sequence>
<id> TAB <sequence>
...
v}
*)

type item = {
  name : string ;
  sequence : string ;
}

type t = private {
  nb_sequences : int ;
  nb_cols : int ;
  items : item array ;
}

val of_file : string -> (t, [> `Msg of string]) result
