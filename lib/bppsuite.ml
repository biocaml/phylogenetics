open Core

type alphabet =
  | DNA
  | RNA
  | Protein
  | Binary
  | Word of { letter : [`DNA | `RNA | `Protein] ;
              length : int }
  | Codon of { letter : [`DNA | `RNA] }

let string_of_letter = function
  | `DNA -> "DNA"
  | `RNA -> "RNA"
  | `Protein -> "Protein"

let string_of_alphabet = function
  | DNA -> "DNA"
  | RNA -> "RNA"
  | Protein -> "Protein"
  | Binary -> "Binary"
  | Word { letter ; length } ->
    sprintf "Word(letter=%s, length=%d)"
      (string_of_letter letter) length
  | Codon { letter } ->
    sprintf "Codon(letter=%s)" (string_of_letter letter)

type model =
  | JC69
  | K80 of { kappa : float option }

let string_of_model = function
  | JC69 -> "JC69"
  | K80 { kappa } ->
    Option.value_map kappa ~default:"" ~f:(sprintf "kappa=%f")
    |> sprintf "K80(%s)"

module Cmd = struct
  let bppml ~alphabet ~model ~input_tree_file ~input_sequence_file ?output_tree_file () =
    Printf.sprintf
      "bppml \
       input.tree.file=%s \
       input.sequence.file=%s \
       alphabet=\"%s\" \
       model=\"%s\" \
       %s \
       optimization=None"
      input_tree_file input_sequence_file
      (string_of_alphabet alphabet)
      (string_of_model model)
      (Option.value_map output_tree_file ~default:"" ~f:(sprintf "output.tree.file=%s"))

  let bppseqgen ~alphabet ~model ~number_of_sites ~input_tree_file ~output_sequence_file =
    Printf.sprintf
      "bppseqgen \
       input.tree.file=%s \
       output.sequence.file=%s \
       alphabet=\"%s\" \
       model=\"%s\" \
       number_of_sites=%d"
      input_tree_file output_sequence_file (string_of_alphabet alphabet)
      (string_of_model model) number_of_sites
end
