open Base

module type S = sig
  include Alphabet.S_int

  val to_string : t -> string
  val of_string : string -> t option
  val neighbours : t -> t -> (int * Nucleotide.t * Nucleotide.t) option
  val nucleotides : t -> Nucleotide.t * Nucleotide.t * Nucleotide.t
end

module type Genetic_code = sig
  type codon
  val stop_codons : codon list
  val aa_of_codon : codon -> Amino_acid.t option
  val synonym : codon -> codon -> bool

  module NS : sig
    include S
    val to_codon : t -> codon
    val aa_of_codon : t -> Amino_acid.t
    val synonym : t -> t -> bool
    val of_int_exn : int -> t
  end
end

module Impl(X : sig val triplets : string array end) = struct
  include Alphabet.Make(struct let card = Array.length X.triplets end)

  let nucleotides = Array.map X.triplets ~f:(fun c ->
      Array.init (String.length c) ~f:(fun i -> Nucleotide.of_char_exn c.[i])
    )

  let hash_table =
    Array.to_list X.triplets
    |> List.mapi ~f:(fun i t -> t, i)
    |> Core_kernel.String.Table.of_alist_exn

  let to_string i = X.triplets.(i)
  let of_string s = Hashtbl.find hash_table s

  let neighbours p q =
    let p_s = X.triplets.(p) and q_s = X.triplets.(q) in
    match Char.(p_s.[0] = q_s.[0], p_s.[1] = q_s.[1], p_s.[2] = q_s.[2]) with
    | false, true, true -> Some (0, nucleotides.(p).(0), nucleotides.(q).(0))
    | true, false, true -> Some (1, nucleotides.(p).(1), nucleotides.(q).(1))
    | true, true, false -> Some (2, nucleotides.(p).(2), nucleotides.(q).(2))
    | _ -> None

  let%test _ = Poly.(neighbours 0 5 = None)

  let nucleotides p = nucleotides.(p).(0), nucleotides.(p).(1), nucleotides.(p).(2)

  let to_codon x = x
end

let all_triplets = [|
  "TTT"; "TTC"; "TTA"; "TTG"; "TCT"; "TCC"; "TCA"; "TCG"; "TAT"; "TAC";
  "TAA"; "TAG"; "TGT"; "TGC"; "TGA"; "TGG"; "CTT"; "CTC"; "CTA"; "CTG"; "CCT"; "CCC"; "CCA";
  "CCG"; "CAT"; "CAC"; "CAA"; "CAG"; "CGT"; "CGC"; "CGA"; "CGG"; "ATT"; "ATC"; "ATA"; "ATG";
  "ACT"; "ACC"; "ACA"; "ACG"; "AAT"; "AAC"; "AAA"; "AAG"; "AGT"; "AGC"; "AGA"; "AGG"; "GTT";
  "GTC"; "GTA"; "GTG"; "GCT"; "GCC"; "GCA"; "GCG"; "GAT"; "GAC"; "GAA"; "GAG"; "GGT"; "GGC";
  "GGA"; "GGG"
|]

module Universal_genetic_code = struct
  let code_array =
    [| 4; 4; 9; 9; 15; 15; 15; 15; 19; 19; -1; -1; 1; 1; -1; 18; 9; 9; 9; 9;
       12; 12; 12; 12; 6; 6; 13; 13; 14; 14; 14; 14; 7; 7; 7; 10; 16; 16; 16; 16; 11; 11; 8; 8; 15; 15;
       14; 14; 17; 17; 17; 17; 0; 0; 0; 0; 2; 2; 3; 3; 5; 5; 5; 5 |]

  let code = Array.map ~f:Amino_acid.of_int code_array
  let aa_of_codon i = code.(i)
  let stop_codons = [ 10 ; 11 ; 14 ]
  let synonym p q = Poly.(code.(p) = code.(q))

  module NS = struct
    let triplets = Array.filteri all_triplets ~f:(fun i _ -> code_array.(i) >= 0)
    let code_array = Array.filter code_array ~f:(fun i -> i >= 0)
    let code = Array.map ~f:Amino_acid.of_int_exn code_array
    include Impl(struct let triplets = triplets end)
    let aa_of_codon i = code.(i)
    let synonym p q = Poly.(code.(p) = code.(q))
    let of_int_exn i =
      if i < 0 || i >= card then raise (Invalid_argument "Codon.Universal_genetic_code.NS.of_int_exn")
      else i
  end
end

include Impl(struct let triplets = all_triplets end)
