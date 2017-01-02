open Sigs
open Biocaml_ez
open Core_kernel.Std


module Make_hashtbl (S:SEQUENCE) = struct
  type base = S.base
  type sequence = S.t
  type t = (string, S.t) Hashtbl.t

  let of_assoc_list l =
    let align = Int.Table.create ~size:(List.length l) () in
    List.iter ~f:(fun (i,s) -> Hashtbl.add_exn ~key:i ~data:s align) l
end


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
