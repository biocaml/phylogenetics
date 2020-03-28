open Core_kernel

type 'a t = Cons of 'a * 'a list

let init n ~f =
  if n < 1 then invalid_arg "Non_empty_list should be at least of length 1" ;
  Cons (f 0, List.init (n - 1) ~f:(fun i -> f (i + 1)))

let cons h t = Cons (h, t)

let fold (Cons (h, t)) ~init ~f =
  List.fold t ~f ~init:(f init h)

let map (Cons (h, t)) ~f =
  cons (f h) (List.map t ~f)

let map2_exn (Cons (h1, t1)) (Cons (h2, t2)) ~f =
  cons (f h1 h2) (List.map2_exn t1 t2 ~f)

let iter (Cons (h, t)) ~f =
  f h ; List.iter t ~f

let to_list (Cons (h, t)) =
  h :: t

let filter_map (Cons (h, t)) ~f =
  match f h, List.filter_map t ~f with
  | None, [] -> None
  | None, h :: t -> Some (Cons (h, t))
  | Some h, l -> Some (Cons (h, l))

let unzip (Cons ((h1, h2), t)) =
  let t1, t2 = List.unzip t in
  Cons (h1, t1), Cons (h2, t2)