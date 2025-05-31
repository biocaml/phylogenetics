module Result = struct
  let (let*) = Result.bind
  let (let+) x f = Result.map f x
end

module Option = struct
  let (let*) = Option.bind
  let (let+) x f = Option.map f x
  let (and+) x y =
    match x, y with
    | Some x, Some y -> Some (x, y)
    | Some _, None
    | None, Some _
    | None, None -> None
end
