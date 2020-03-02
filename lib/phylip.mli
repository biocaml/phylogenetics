(** The original format is described
   {{:http://evolution.genetics.washington.edu/phylip.html}there}. This
   implementation also allows a somewhat more relaxed syntax:
{v
<nb sequences> <nb cols>
<id> TAB <sequence>
<id> TAB <sequence>
...
v}
    which is specified with the option [~strict:false].
*)

type item = {
  name : string ;
  sequence : string ;
}

type t = private {
  number_of_sequences : int ;
  sequence_length : int ;
  items : item list ;
}

val read :
  ?strict:bool ->
  string ->
  (t, [> `Msg of string]) result

val read_exn :
  ?strict:bool ->
  string ->
  t
