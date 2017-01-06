open Core_kernel.Std

module Make (E:Sigs.EVOL_MODEL) = struct
  include Seqgen.Make (E)

  let simulate p align treesize =
    let random_tree = TopoTree.make_random treesize in
    Align.equal (seqgen p random_tree 1) align
end

module JC69ri = Make(Models.JC69)

let myalign = JC69ri.seqgen () (TopoTree.make_random 5) 1

let test () =
  List.init 100000 ~f:(fun _ ->
      JC69ri.simulate () myalign 5)
|> List.fold ~init:0 ~f:(fun acc e->if e then acc+1 else acc)
