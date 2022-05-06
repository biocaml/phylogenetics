open Core

type 'a t = Cons of 'a * 'a list

let length (Cons (_, xs)) = 1 + List.length xs

let init n ~f =
  if n < 1 then invalid_arg "Non_empty_list should be at least of length 1" ;
  Cons (f 0, List.init (n - 1) ~f:(fun i -> f (i + 1)))

let cons h t = Cons (h, t)

let fold (Cons (h, t)) ~init ~f =
  List.fold t ~f ~init:(f init h)

let fold_right (Cons (h, t)) ~init ~f =
  f h (List.fold_right t ~f ~init)

let reduce (Cons (h, t)) ~f =
  List.reduce_exn (h :: t) ~f

let map (Cons (h, t)) ~f =
  cons (f h) (List.map t ~f)

let mapi (Cons (h, t)) ~f =
  cons (f 0 h) (List.mapi t ~f:(fun i -> f (i + 1)))

let map2_exn (Cons (h1, t1)) (Cons (h2, t2)) ~f =
  cons (f h1 h2) (List.map2_exn t1 t2 ~f)

let iter (Cons (h, t)) ~f =
  f h ; List.iter t ~f

let to_list (Cons (h, t)) =
  h :: t

let filter_map (Cons (h, t)) ~f =
  match f h, List.filter_map t ~f with
  | None, [] -> []
  | None, (_ :: _ as r) -> r
  | Some h, l -> h :: l

let filter (Cons (h, t)) ~f =
  match f h, List.filter t ~f with
  | false, [] -> []
  | false, (_ :: _ as r) -> r
  | true, l -> h :: l

let unzip (Cons ((h1, h2), t)) =
  let t1, t2 = List.unzip t in
  Cons (h1, t1), Cons (h2, t2)

let for_all (Cons (h, t)) ~f =
  f h && List.for_all t ~f

let exists (Cons (h, t)) ~f =
  f h || List.exists t ~f

let hd (Cons (h, _)) = h

let singleton x = Cons (x, [])

let of_list = function
  | [] -> None
  | h :: t -> Some (Cons (h, t))

let of_list_exn = function
  | [] -> failwith "empty list"
  | h :: t -> Cons (h, t)

let sort (Cons (h, t)) ~compare =
  match List.sort (h:: t) ~compare with
  | h :: t -> Cons (h, t)
  | [] -> assert false
