open Core_kernel

type 'a t = Cons of 'a * 'a list

let cons h t = Cons (h, t)

let fold (Cons (h, t)) ~init ~f =
  List.fold t ~f ~init:(f init h)

let map (Cons (h, t)) ~f =
  cons (f h) (List.map t ~f)

let to_list (Cons (h, t)) =
  h :: t
