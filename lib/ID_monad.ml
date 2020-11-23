type 'a t = int -> 'a * int

let return x = fun s -> x, s

let (let*) (m : 'a t) (f : 'a -> 'b t) : 'b t  =
  fun state ->
  let y, state' = m state in
  f y state'

let (let+) (m : 'a t) (f : 'a -> 'b) : 'b t  =
  fun state ->
  let y, state' = m state in
  f y, state'

let new_id id = (id, id + 1)

let run m = fst (m 0)
