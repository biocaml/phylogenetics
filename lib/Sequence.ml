type 'a sequence = (int*'a) list

let get_base = List.assoc

let test_get_base () =
  let open DNA in
  let myseq = [(0,A);(1,T);(2,C);(3,T)] in
  get_base 2 myseq |> string_of_dna

