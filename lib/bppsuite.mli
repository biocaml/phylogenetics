type alphabet =
  | DNA
  | RNA
  | Protein
  | Binary
  | Word of { letter : [`DNA | `RNA | `Protein] ;
              length : int }
  | Codon of { letter : [`DNA | `RNA] }

val string_of_alphabet : alphabet -> string

type model =
  | JC69
  | K80 of { kappa : float option }

val string_of_model : model -> string

module Cmd : sig
  val bppml :
    alphabet:alphabet ->
    model:model ->
    input_tree_file:string ->
    input_sequence_file:string ->
    ?output_tree_file:string ->
    unit ->
    string

  val bppseqgen :
    alphabet:alphabet ->
    model:model ->
    number_of_sites:int ->
    input_tree_file:string ->
    output_sequence_file:string ->
    string
end
