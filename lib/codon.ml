open Base

module type S = sig
  include Alphabet.S_int

  val to_string : t -> string
  val of_string : string -> t option
  val neighbours : t -> t -> bool
  val neighbours_diff : t -> t -> (int * Nucleotide.t * Nucleotide.t) option
  val nucleotides : t -> Nucleotide.t * Nucleotide.t * Nucleotide.t
end

module Impl(X : sig val triplets : string array end) = struct
  include Alphabet.Make(struct let card = Array.length X.triplets end)

  let nucleotides = Array.map X.triplets ~f:(fun c ->
      Array.init (String.length c) ~f:(fun i -> Nucleotide.of_char_exn c.[i])
    )

  let hash_table =
    Array.to_list X.triplets
    |> List.mapi ~f:(fun i t -> t, i)
    |> Core.String.Table.of_alist_exn

  let to_string i = X.triplets.(i)
  let of_string s = Hashtbl.find hash_table s

  let neighbours p q =
    let p_s = X.triplets.(p) and q_s = X.triplets.(q) in
    match Char.(p_s.[0] = q_s.[0], p_s.[1] = q_s.[1], p_s.[2] = q_s.[2]) with
    | false, true, true
    | true, false, true
    | true, true, false -> true
    | _ -> false

  let neighbours_diff p q =
    let p_s = X.triplets.(p) and q_s = X.triplets.(q) in
    match Char.(p_s.[0] = q_s.[0], p_s.[1] = q_s.[1], p_s.[2] = q_s.[2]) with
    | false, true, true -> Some (0, nucleotides.(p).(0), nucleotides.(q).(0))
    | true, false, true -> Some (1, nucleotides.(p).(1), nucleotides.(q).(1))
    | true, true, false -> Some (2, nucleotides.(p).(2), nucleotides.(q).(2))
    | _ -> None

  let%test _ = Poly.(neighbours_diff 0 5 = None)

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

let decode_ncbi_string s =
  let n = String.length s in
  Array.init n ~f:(fun i ->
      match Amino_acid.of_char s.[i] with
      | Some aa -> Amino_acid.to_int aa
      | None -> -1
    )

type genetic_code = int * string * string

(* adapted from https://www.ncbi.nlm.nih.gov/Taxonomy/Utils/wprintgc.cgi?mode=c *)

let genetic_codes = [
  1, "Standard", "FFLLSSSSYY**CC*WLLLLPPPPHHQQRRRRIIIMTTTTNNKKSSRRVVVVAAAADDEEGGGG" ;
  2, "Vertebrate Mitochondrial", "FFLLSSSSYY**CCWWLLLLPPPPHHQQRRRRIIMMTTTTNNKKSS**VVVVAAAADDEEGGGG" ;
  3, "Yeast Mitochondrial", "FFLLSSSSYY**CCWWTTTTPPPPHHQQRRRRIIMMTTTTNNKKSSRRVVVVAAAADDEEGGGG" ;
  4, "Mold, Protozoan, and Coelenterate Mitochondrial Code and the Mycoplasma/Spiroplasma", "FFLLSSSSYY**CCWWLLLLPPPPHHQQRRRRIIIMTTTTNNKKSSRRVVVVAAAADDEEGGGG" ;
  5, "Invertebrate Mitochondrial", "FFLLSSSSYY**CCWWLLLLPPPPHHQQRRRRIIMMTTTTNNKKSSSSVVVVAAAADDEEGGGG" ;
  6, "Ciliate, Dasycladacean and Hexamita Nuclear", "FFLLSSSSYYQQCC*WLLLLPPPPHHQQRRRRIIIMTTTTNNKKSSRRVVVVAAAADDEEGGGG" ;
  9, "Echinoderm and Flatworm Mitochondrial", "FFLLSSSSYY**CCWWLLLLPPPPHHQQRRRRIIIMTTTTNNNKSSSSVVVVAAAADDEEGGGG" ;
  10, "Euplotid Nuclear", "FFLLSSSSYY**CCCWLLLLPPPPHHQQRRRRIIIMTTTTNNKKSSRRVVVVAAAADDEEGGGG" ;
  11, "Bacterial, Archaeal and Plant Plastid", "FFLLSSSSYY**CC*WLLLLPPPPHHQQRRRRIIIMTTTTNNKKSSRRVVVVAAAADDEEGGGG" ;
  12, "Alternative Yeast Nuclear", "FFLLSSSSYY**CC*WLLLSPPPPHHQQRRRRIIIMTTTTNNKKSSRRVVVVAAAADDEEGGGG" ;
  13, "Ascidian Mitochondrial", "FFLLSSSSYY**CCWWLLLLPPPPHHQQRRRRIIMMTTTTNNKKSSGGVVVVAAAADDEEGGGG" ;
  14, "Alternative Flatworm Mitochondrial", "FFLLSSSSYYY*CCWWLLLLPPPPHHQQRRRRIIIMTTTTNNNKSSSSVVVVAAAADDEEGGGG" ;
  16, "Chlorophycean Mitochondrial", "FFLLSSSSYY*LCC*WLLLLPPPPHHQQRRRRIIIMTTTTNNKKSSRRVVVVAAAADDEEGGGG" ;
  21, "Trematode Mitochondrial", "FFLLSSSSYY**CCWWLLLLPPPPHHQQRRRRIIMMTTTTNNNKSSSSVVVVAAAADDEEGGGG" ;
  22, "Scenedesmus obliquus Mitochondrial", "FFLLSS*SYY*LCC*WLLLLPPPPHHQQRRRRIIIMTTTTNNKKSSRRVVVVAAAADDEEGGGG" ;
  23, "Thraustochytrium Mitochondrial", "FF*LSSSSYY**CC*WLLLLPPPPHHQQRRRRIIIMTTTTNNKKSSRRVVVVAAAADDEEGGGG" ;
  24, "Rhabdopleuridae Mitochondrial", "FFLLSSSSYY**CCWWLLLLPPPPHHQQRRRRIIIMTTTTNNKKSSSKVVVVAAAADDEEGGGG" ;
  25, "Candidate Division SR1 and Gracilibacteria", "FFLLSSSSYY**CCGWLLLLPPPPHHQQRRRRIIIMTTTTNNKKSSRRVVVVAAAADDEEGGGG" ;
  26, "Pachysolen tannophilus Nuclear", "FFLLSSSSYY**CC*WLLLAPPPPHHQQRRRRIIIMTTTTNNKKSSRRVVVVAAAADDEEGGGG" ;
  27, "Karyorelict Nuclear", "FFLLSSSSYYQQCCWWLLLLPPPPHHQQRRRRIIIMTTTTNNKKSSRRVVVVAAAADDEEGGGG" ;
  28, "Condylostoma Nuclear", "FFLLSSSSYYQQCCWWLLLLPPPPHHQQRRRRIIIMTTTTNNKKSSRRVVVVAAAADDEEGGGG" ;
  29, "Mesodinium Nuclear", "FFLLSSSSYYYYCC*WLLLLPPPPHHQQRRRRIIIMTTTTNNKKSSRRVVVVAAAADDEEGGGG" ;
  30, "Peritrich Nuclear", "FFLLSSSSYYEECC*WLLLLPPPPHHQQRRRRIIIMTTTTNNKKSSRRVVVVAAAADDEEGGGG" ;
  31, "Blastocrithidia Nuclear", "FFLLSSSSYYEECCWWLLLLPPPPHHQQRRRRIIIMTTTTNNKKSSRRVVVVAAAADDEEGGGG" ;
  33, "Cephalodiscidae Mitochondrial UAA-Tyr", "FFLLSSSSYYY*CCWWLLLLPPPPHHQQRRRRIIIMTTTTNNKKSSSKVVVVAAAADDEEGGGG" ;
]

let transl_table (i, _, _) = i
let label_of_genetic_code (_, l, _) = l

include Impl(struct let triplets = all_triplets end)

module type Genetic_code = sig
  type codon = t
  val stop_codons : codon list
  val is_stop_codon : codon -> bool
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

module Genetic_code_impl(X : sig val code_array : int array end) = struct
  open X

  type codon = t
  let code = Array.map ~f:Amino_acid.of_int code_array
  let aa_of_codon i = code.(i)
  let stop_codons =
    Array.filter_mapi code ~f:(fun i x -> if Option.is_none x then Some i else None)
  let is_stop_codon c = Array.mem stop_codons c ~equal:( = )
  let stop_codons = Array.to_list stop_codons
  let synonym p q = Poly.(code.(p) = code.(q))

  module NS = struct
    let triplets = Array.filteri all_triplets ~f:(fun i _ -> code_array.(i) >= 0)
    let code_array = Array.filter code_array ~f:(fun i -> i >= 0)
    let code = Array.map ~f:Amino_acid.of_int_exn code_array
    include Impl(struct let triplets = triplets end)
    let aa_of_codon i = code.(i)
    let synonym p q = Poly.(code.(p) = code.(q))
    let of_int_exn i =
      if i < 0 || i >= card then raise (Invalid_argument "Codon.Genetic_code_impl.NS.of_int_exn")
      else i
  end
end

let genetic_code_impl (_, _, code) =
  let module X = struct
    let code_array = decode_ncbi_string code
  end
  in
  let module M = Genetic_code_impl(X) in
  (module M : Genetic_code)

module Universal_genetic_code = struct
  let m = genetic_code_impl (List.hd_exn genetic_codes)
  include (val m)

  (* probably useless optimization *)
  let is_stop_codon c = c = 10 || c = 11 || c = 14
end

let%test "universal code" =
  let s =
    Array.map all_triplets ~f:of_string
    |> Array.to_list
    |> Option.all
    |> Option.value_exn
    |> List.map ~f:(fun c ->
        Universal_genetic_code.aa_of_codon c
        |> Option.value_map ~default:'*' ~f:Amino_acid.to_char
      )
    |> String.of_char_list
  in
  String.equal s "FFLLSSSSYY**CC*WLLLLPPPPHHQQRRRRIIIMTTTTNNKKSSRRVVVVAAAADDEEGGGG"
