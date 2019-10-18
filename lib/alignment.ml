open Core_kernel
open Biocaml_ez (* for fasta parsing *)

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
    Fasta.with_file filename ~f:(fun _ stream -> (* using biocaml_ez to get a stream of fasta sequences *)
        CFStream.Stream.iter ~f:(fun item -> (* iterating on said stream *)
            (*  stream element is a record {sequence:string; description:string} *)
            let data = S.of_string_exn item.Fasta.sequence in
            Hashtbl.add_exn ~key:item.Fasta.description ~data:data align) stream
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

  let equal x y = Hashtbl.equal x y (=)
end
