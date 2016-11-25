open DNA

type base = dna
type sequence = (int*base) list

let get_base = List.assoc
