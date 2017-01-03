open Sigs

module Make (B:BASE): SEQUENCE with type base = B.t

module Make_list (B:BASE): SEQUENCE with type base = B.t

module DNA: module type of Make(Nucleotide)
