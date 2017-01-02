open Sigs
open Biocaml_ez
open Core_kernel.Std


module Make_hashtbl (S:SEQUENCE) = struct
  type base = S.base
  type sequence = S.t
  type t = (int, S.t) Hashtbl.t
  module Sequence = S

  let get_base tab ~seq ~pos = S.get (Hashtbl.find_exn tab seq) pos

  let of_assoc_list l =
    let align = Int.Table.create ~size:(List.length l) () in
    List.iter ~f:(fun (i,s) -> Hashtbl.add_exn ~key:i ~data:s align) l ;
    align

  let of_string_list l =
    let align = Int.Table.create ~size:(List.length l) () in
    List.iteri ~f:(fun i s -> Hashtbl.add_exn ~key:i ~data:(S.of_string s) align) l ;
    align

  let of_fasta filename =
    let align = Int.Table.create ~size:10 () in (* placeholder size *)
    Fasta.with_file filename ~f:(fun _ stream ->
        CFStream.Stream.iter ~f:(fun item ->
            let key = Scanf.sscanf item.Biocaml_ez.Fasta.description "T%d" (fun x->x) in
            let data = S.of_string item.Biocaml_ez.Fasta.sequence in
            Hashtbl.add_exn ~key:key ~data:data align) stream
      ) ;
    align
end

module DNA_align = Make_hashtbl (Seq.DNA)
let myalign = DNA_align.of_assoc_list [
    (0, DNA_align.Sequence.of_string "ATTC");
    (1, DNA_align.Sequence.of_string "TGCA")
  ]
let myalign2 = DNA_align.of_string_list ["ATTC"; "TGCA"]
let myalign3 = DNA_align.of_fasta "test_data/tiny1.fasta"
let test () = DNA_align.get_base ~seq:1 ~pos:0 myalign3


module Make (S:SEQUENCE) = struct
  type base = S.base
  type sequence = S.t
  type t = (int * sequence) list

  let get_base tab ~seq ~pos = S.get (ListLabels.assoc seq tab) pos

  let of_string_list l =
    let rec aux acc i = function
      | [] -> List.rev acc
      | s::t -> aux ((i, (S.of_string s))::acc) (i+1) t
    in aux [] 0 l

  let of_assoc_list l = l

  let of_fasta filename = Fasta.with_file filename ~f:(
      fun _ stream ->
        CFStream.Stream.to_list stream
        |> List.map ~f:(fun item -> (Scanf.sscanf item.Biocaml_ez.Fasta.description "T%d" (fun x->x), S.of_string item.Biocaml_ez.Fasta.sequence))
    )

  let check_lengths = function
    | [] -> true
    | (_,h)::t -> let l=S.length h in
      List.fold t ~init:true ~f:(fun acc (_,x) -> (S.length x = l) && acc)

  let length = function
    | [] -> 0
    | (_,h)::_ as l -> if check_lengths l then S.length h else failwith "Uneven sequences in alignment"

  let pp fmt tab =
    List.map ~f:(function (x,y) -> Printf.sprintf "%d: %s" x (S.to_string y)) tab
    |> String.concat ~sep:" ; "
    |> Format.fprintf fmt "%s"

  let to_file seq name =
    List.map seq ~f:(fun (i,seq) -> Printf.sprintf ">T%d\n%s" i (S.to_string seq))
    |> Out_channel.write_lines name
end

(* module DNASeq = Make (Seq.DNA) *)
(* let myseq = DNASeq.of_string_list ["ATTGTC"; "AGGACC"] *)
(* let test () = DNASeq.to_file myseq "tmp.seq" *)
