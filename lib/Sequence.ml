open DNA

type base = dna
type sequence = (int*base) list

let get_base = List.assoc

let test () =
  let myseq = [(0,A);(1,T);(2,C);(3,G);(4,A)] in
  get_base 2 myseq |> DNA.string_of_dna ;;
