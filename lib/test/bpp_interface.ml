open Core_kernel.Std

(*  fails adter printing the content of a file with a message*)
let fail_file ?(path="tmp.data") message =
  Printf.sprintf "ERROR (bpp_interface): %s:\n%s"
    message (In_channel.read_all path) |> prerr_endline;
  failwith message

let felsenstein_bpp ?(alphabet="DNA") ?(model="JC69") ?(path=".") ~tree seq =
  let script = Printf.sprintf
      "bppml \
       input.tree.file=%s/%s \
       input.sequence.file=%s/%s \
       alphabet=%s \
       model=%s \
       output.tree.file=tmp.tree \
       optimization=None \
       &> tmp.data"
      path tree path seq alphabet model
  in
  begin
    Out_channel.write_all "tmp.sh" ~data:script ;
    if Sys.command "bash tmp.sh" <> 0 then fail_file "bppml failed" ;
    match
      In_channel.read_lines "tmp.data"
      |> List.filter ~f:(fun l->String.prefix l 11 = "Initial log")
    with
    | [l] ->
      Scanf.sscanf l "Initial log likelihood.................: %f" (fun x->x)
    | _ -> fail_file "unexpected bppml output"
  end

let seqgen_bpp ?(alphabet="DNA") ?(model="JC69") ?(path=".") ~tree output size =
  let script = Printf.sprintf
      "bppseqgen \
       input.tree.file=%s/%s \
       output.sequence.file=%s/%s \
       alphabet=%s \
       model=%s \
       number_of_sites=%d \
       &> tmp.data"
      path tree path output alphabet model size
  in
  begin
    Out_channel.write_all "tmp.sh" ~data:script ;
    match Sys.command "bash tmp.sh" with
    | 0 -> ()
    | _ -> fail_file "bppseqgen failed"
  end

let test () = felsenstein_bpp
    ~path:"test_dataz"
    ~tree:"small_1.tree"
    "small_1.seq"

let test2 () = seqgen_bpp
    ~path:"test_data"
    ~tree:"small_1.tree"
    "tmp.seq"
    15
