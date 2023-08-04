open Core

type t = {
  descriptions : string array ;
  sequences : string array ;
}

let sequence t i = t.sequences.(i)

let description t i = t.descriptions.(i)

let nrows a = Array.length a.sequences
let ncols a = String.length a.sequences.(0)


type error = [
  | `Empty_alignment
  | `Unequal_sequence_lengths
]
[@@deriving show]

let check_non_empty_list = function
  | [] -> Error (`Empty_alignment)
  | items -> Ok items

let of_tuples items =
  let descriptions, sequences = Array.unzip items in
  let n = String.length sequences.(0) in
  if (Array.for_all sequences ~f:(fun s -> String.length s = n))
  then Ok { descriptions ; sequences  ; }
  else Error `Unequal_sequence_lengths

let of_assoc_list l =
  match check_non_empty_list l with
  | Ok items -> Array.of_list items |> of_tuples
  | Error e -> Error e

let map t ~f =
  Array.map2_exn t.descriptions t.sequences
    ~f:(fun description sequence -> f ~description ~sequence)
  |> of_tuples

let array_mapi t ~f =
  Array.mapi t.descriptions
    ~f:(fun i description -> f i ~description ~sequence:t.sequences.(i))

let fold t ~init ~f = Array.fold2_exn t.descriptions t.sequences ~init
    ~f:(fun acc description sequence -> f acc ~description ~sequence)

module Fasta = struct

  type parsing_error = [
    | `Fasta_parser_error of string
    | error
  ]
  [@@deriving show]

  let of_fasta_items (items:Biotk.Fasta.item list) =
    List.map items ~f:(fun x -> x.description, x.sequence)
    |> of_assoc_list

  let from_file fn =
    let open Biotk.Let_syntax.Result in
    let* _, items =
      Biotk.Fasta.from_file fn
      |> Result.map_error ~f:(fun msg -> `Fasta_parser_error msg)
    in
    of_fasta_items items

  let from_file_exn fn =
    match from_file fn with
    | Ok al -> al
    | Error e -> failwith (show_parsing_error e)

  let to_channel ({sequences ; descriptions}) oc =
    Array.iter2_exn descriptions sequences ~f:(fun desc seq ->
        Out_channel.output_lines oc [
          sprintf ">%s" desc ;
          seq;
        ])

  let to_file al fn =
    Out_channel.with_file fn ~f:(to_channel al)
end

module Phylip = struct
  type parsing_error = [
    | `Phylip_parser_error of string
    | error
  ]
  [@@deriving show]

  let of_phylip { Phylip.items ; _ } =
    List.map items ~f:(fun { name ; sequence } -> name, sequence)
    |> of_assoc_list

  let to_phylip ({sequences ; descriptions} as ali) =
    let number_of_sequences = nrows ali in
    let items = List.init number_of_sequences ~f:(fun i ->
        { Phylip.name = descriptions.(i) ; sequence = sequences.(i) }
      ) in
    Phylip.make_exn items

  let from_file ?strict fn =
    let open Biotk.Let_syntax.Result in
    let* phylip =
      Phylip.read ?strict fn
      |> Result.map_error ~f:(fun (`Msg msg) -> `Phylip_parser_error msg)
    in
    of_phylip phylip

  let from_file_exn ?strict fn =
    match from_file ?strict fn with
    | Ok al -> al
    | Error e -> failwith (show_parsing_error e)

end

let find_sequence t id =
  Array.findi t.descriptions ~f:(fun _ x -> String.equal x id)
  |> Option.map ~f:(fun (i, _) -> t.sequences.(i))

let indel_free_columns ali =
  Array.init (nrows ali) ~f:(fun j ->
      Array.for_all ali.sequences ~f:(fun s -> Char.(s.[j] <> '-'))
    )

let residues al ~column:j =
  if j < 0 || j > ncols al then raise (Invalid_argument "Alignment.residues") ;
  Array.fold al.sequences ~init:Char.Set.empty ~f:(fun acc s -> Char.Set.add acc s.[j])

let number_of_residues_per_column_stats al =
  let x =
    Array.init (ncols al) ~f:(fun column ->
        residues al ~column
        |> Char.Set.length
      )
  in
  Binning.counts (Caml.Array.to_seq x)
  |> Caml.List.of_seq

let composition al =
  let module C = Binning.Counter in
  let acc = C.create () in
  let n = float (nrows al * ncols al) in
  Array.iter al.sequences ~f:(fun s ->
      String.iter s ~f:(fun c -> C.tick acc c)
    ) ;
  Caml.Seq.map (fun (c, k) -> (c, float k /. n)) (Binning.seq acc)
  |> Caml.List.of_seq

let constant_site al j =
  let m = nrows al in
  let rec find_state i =
    if i < m then
      match al.sequences.(i).[j] with
      | '-' -> find_state (i + 1)
      | c -> find_other_state c (i + 1)
    else true
  and find_other_state c i =
    if i < m then
      match al.sequences.(i).[j] with
      | '-' -> find_other_state c (i + 1)
      | c' when Char.equal c c' -> find_other_state c (i + 1)
      | _ -> false
    else true
  in
  find_state 0

open Core

module Make(S : Seq.S) = struct
  type base = S.base
  type sequence = S.t
  type index = string
  type t = (index, sequence) Hashtbl.t
  module Sequence = S

  let get_base tab ~seq ~pos = S.get (Hashtbl.find_exn tab seq) pos

  let of_assoc_list l =
    let align = String.Table.create ~size:(List.length l) () in
    List.iter ~f:(fun (i,s) -> Hashtbl.add_exn ~key:i ~data:s align) l ;
    align

  let of_string_list l =
    let align = String.Table.create ~size:(List.length l) () in
    (* arbitrarily indexes sequences by string Ti where i is an integer;
       this mimics the format used by bppseqgen *)
    List.iteri ~f:(fun i s -> Hashtbl.add_exn ~key:(Printf.sprintf "T%d" i) ~data:(S.of_string_exn s) align) l ;
    align

  let of_fasta filename =
    let align = String.Table.create ~size:10 () in (* placeholder size TODO *)
    let _, items = Biotk.Fasta.from_file_exn filename in
    List.iter items ~f:(fun item ->
        let data = S.of_string_exn item.Biotk.Fasta.sequence in
        Hashtbl.add_exn ~key:item.Biotk.Fasta.description ~data:data align
      ) ;
    align

  let length x = (* this is the length of the sequences, not the nb of sequences! *)
    if Hashtbl.is_empty x then invalid_arg "empty alignment"
    else Hashtbl.fold x ~init:0 ~f:(fun ~key:_ ~data acc ->
        let l = S.length data in
        if l=0 then invalid_arg "alignment with empty sequence"
        else if acc<>0 && acc<>l then invalid_arg "sequence length mismatch"
        else l (* returns only if all lengths were equal *)
      )

  let nb_seq x = Hashtbl.length x

  let pp fmt x =
    Hashtbl.to_alist x
    |> List.map ~f:(fun (i,s) -> Printf.sprintf "%s: %s" i (S.to_string s))
    |> String.concat ~sep:"\n"
    |> Format.fprintf fmt "%s"

  let to_file x filename =
    Hashtbl.to_alist x
    |> List.map ~f:(fun (i,s) -> Printf.sprintf ">%s\n%s" i (S.to_string s))
    |> Out_channel.write_lines filename

  let equal (x : t) y = Hashtbl.equal Poly.equal x y
end
