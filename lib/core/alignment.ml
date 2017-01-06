open Sigs
open Biocaml_ez
open Core_kernel.Std


module Make (S:SEQUENCE) = struct
  type base = S.base
  type sequence = S.t
  type t = (string, sequence) Hashtbl.t
  module Sequence = S

  let get_base tab ~seq ~pos = S.get (Hashtbl.find_exn tab seq) pos

  let of_assoc_list l =
    let align = String.Table.create ~size:(List.length l) () in
    List.iter ~f:(fun (i,s) -> Hashtbl.add_exn ~key:i ~data:s align) l ;
    align

  let of_string_list l =
    let align = String.Table.create ~size:(List.length l) () in
    List.iteri ~f:(fun i s -> Hashtbl.add_exn ~key:(Printf.sprintf "T%d" i) ~data:(S.of_string s) align) l ;
    align

  let of_fasta filename =
    let align = String.Table.create ~size:10 () in (* placeholder size *)
    Fasta.with_file filename ~f:(fun _ stream ->
        CFStream.Stream.iter ~f:(fun item ->
            let data = S.of_string item.Biocaml_ez.Fasta.sequence in
            Hashtbl.add_exn ~key:item.Biocaml_ez.Fasta.description ~data:data align) stream
      ) ;
    align

  (* give length of sequences in alignment ;
     fails if empty or length mismatch between sequences *)
  let length x =
    if Hashtbl.is_empty x then invalid_arg "empty alignment"
    else Hashtbl.fold x ~init:0 ~f:(fun ~key:_ ~data acc ->
        let l = S.length data in
        if l=0 then invalid_arg "alignment with empty sequence"
        else if acc<>0 && acc<>l then invalid_arg "sequence length mismatch"
        else l
      )

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
