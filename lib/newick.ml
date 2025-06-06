open Core

include Newick_ast

module I = Newick_parser.MenhirInterpreter

let message_of_env env =
  try
    match I.top env with
    | Some (I.Element (s, _, _, _)) ->
      I.number s
      |> Newick_parser_errors.message
      |> Option.return
    | None -> None
  with Stdlib.Not_found -> None

let fail lexbuf (checkpoint : t I.checkpoint) =
  match checkpoint with
  | I.HandlingError env ->
    let msg =
      match message_of_env env with
      | Some msg -> msg
      | None -> "Syntax error"
    in
    Error (mkerror lexbuf msg)
  | _ -> assert false

let loop lexbuf result =
  let supplier = I.lexer_lexbuf_to_supplier Newick_lexer.token lexbuf in
  I.loop_handle Result.return (fail lexbuf) supplier result

let parse lexbuf =
  try loop lexbuf (Newick_parser.Incremental.start lexbuf.lex_curr_p)
  with
  | Newick_lexer.Error (`Newick_parser_error e) ->
    Error (`Newick_parser_error e)

let from_file fn =
  In_channel.with_file fn ~f:(fun ic ->
      parse (Lexing.from_channel ic)
    )

let from_file_exn fn =
  from_file fn
  |> Result.map_error ~f:string_of_error
  |> Result.ok_or_failwith

let from_string s =
  parse (Lexing.from_string s)

let from_string_exn s =
  from_string s
  |> Result.map_error ~f:string_of_error
  |> Result.ok_or_failwith

let rec unparse ({ name ; children ; tags ; parent_branch } : t) =
  let name = Option.value ~default:"" name
  and tags = match tags with
    | [] -> ""
    | xs -> List.map xs ~f:unparse_tag
            |> String.concat
            |> sprintf "[&&NHX%s]"
  in
  let children = match children with
    | [] -> ""
    | xs ->
      List.map xs ~f:unparse
      |> String.concat ~sep:","
      |> sprintf "(%s)"
  in
  let parent_branch = Option.value_map parent_branch ~default:""
      ~f:(fun length ->
          Float.to_string_hum length ~decimals:12 ~strip_zero:true
          |> sprintf ":%s")
  in
  children ^ name ^ parent_branch ^ tags
and unparse_tag (k, v) = sprintf ":%s=%s" k v

let to_string t = unparse t ^ ";"

let to_file t fn =
  Out_channel.write_all fn ~data:(to_string t)

let of_tree
    ?(node_id = Fn.const None) ?(node_tags = Fn.const [])
    ?(leaf_id = Fn.const None) ?(leaf_tags = Fn.const [])
    ?(branch_length = Fn.const None)
    ?parent_branch tree : t =
  let rec node ?parent_branch = function
    | Tree.Node { data ; branches } -> {
        name = node_id data ; tags = node_tags data ; parent_branch ;
        children = List1.map branches ~f:branch |> List1.to_list
      }
    | Tree.Leaf l ->
      { name = leaf_id l ; tags = leaf_tags l ; parent_branch ; children = [] }
  and branch (Branch b) = node ?parent_branch:(branch_length b.data) b.tip
  in
  node ?parent_branch tree

let test_string_equal x y =
  let b = String.(x = y) in
  if not b then fprintf stderr "expected: %s\ngot %s\n" x y ;
  b

let%test "parse . unparse is identity" =
  let data = "(Chrysithr:3.1[&&NHX:Condition=0],((((((((((((((((((Ele.bald:9[&&NHX:Condition=1],Ele.bal2:6[&&NHX:Condition=1]):1[&&NHX:Condition=1],Ele.bal4:6[&&NHX:Condition=1]):1[&&NHX:Condition=1],(Ele.vivi:8[&&NHX:Condition=1],Ele.vivA:6[&&NHX:Condition=1]):3[&&NHX:Condition=1]):50[&&NHX:Condition=1:Transition=1],Ele.bal3:14[&&NHX:Condition=0]):3[&&NHX:Condition=0],Ele.viv2:9[&&NHX:Condition=0]):27[&&NHX:Condition=0],Ele.fici:17[&&NHX:Condition=0]):1[&&NHX:Condition=0],(Ele.grac:3[&&NHX:Condition=0],Ele.lim2:6[&&NHX:Condition=0]):13[&&NHX:Condition=0]):3[&&NHX:Condition=0],(Ele.rost:22[&&NHX:Condition=0],((Ele.limo:4[&&NHX:Condition=0],Ele.pal2:2[&&NHX:Condition=0]):11[&&NHX:Condition=0],(Ele.acut:10[&&NHX:Condition=0],(Ele.palu:6[&&NHX:Condition=0],(Ele.gra2:3[&&NHX:Condition=0],Ele.lim3:2[&&NHX:Condition=0]):0[&&NHX:Condition=0]):4[&&NHX:Condition=0]):5[&&NHX:Condition=0]):8[&&NHX:Condition=0]):2[&&NHX:Condition=0]):2[&&NHX:Condition=0],Ele.geni:27[&&NHX:Condition=0]):10[&&NHX:Condition=0],Ele.quan:33[&&NHX:Condition=0]):32[&&NHX:Condition=0],((Abildgaar:24[&&NHX:Condition=0],Bulbostyl:0.103[&&NHX:Condition=1:Transition=1]):4[&&NHX:Condition=0],(Actinosch:15[&&NHX:Condition=0],(Fimb.lit:16[&&NHX:Condition=0],((Fimb.dic:11[&&NHX:Condition=0],Fimb.fe2:4[&&NHX:Condition=0]):6[&&NHX:Condition=0],(Fimb.li2:46[&&NHX:Condition=1],(Fimb.di2:9[&&NHX:Condition=1],Fimb.fer:4[&&NHX:Condition=1]):17[&&NHX:Condition=1]):66[&&NHX:Condition=1:Transition=1]):2[&&NHX:Condition=0]):34[&&NHX:Condition=0]):19[&&NHX:Condition=0]):28[&&NHX:Condition=0]):10[&&NHX:Condition=0],(Bolboscho:49[&&NHX:Condition=0],((Fuir.abn:19[&&NHX:Condition=0],Fuir.umb:36[&&NHX:Condition=0]):10[&&NHX:Condition=0],(((Scho.lac:4[&&NHX:Condition=0],Scho.val:1[&&NHX:Condition=0]):43[&&NHX:Condition=0],Scho.muc:42[&&NHX:Condition=0]):11[&&NHX:Condition=0],((((Hellmut1:14[&&NHX:Condition=0],Isolepis:25[&&NHX:Condition=0]):9[&&NHX:Condition=0],Hellmut2:31[&&NHX:Condition=0]):7[&&NHX:Condition=0],Scirpoid:29[&&NHX:Condition=0]):4[&&NHX:Condition=0],(Cyp.spha:45[&&NHX:Condition=0],(Cyp.alt3:30[&&NHX:Condition=0],(Cyp.era6:35[&&NHX:Condition=0],((Cyp.era1:19[&&NHX:Condition=0],Cyp.fusc:20[&&NHX:Condition=0]):11[&&NHX:Condition=0],(Cyp.pulc:22[&&NHX:Condition=0],(Cyp.capi:10[&&NHX:Condition=1],(Volkiell:62[&&NHX:Condition=1],(Cyp.ust2:7[&&NHX:Condition=1],(Remirea:13[&&NHX:Condition=1],(Cyp.iria:11[&&NHX:Condition=1],((Killinga:0.217[&&NHX:Condition=1],Pycreus:29[&&NHX:Condition=1]):43[&&NHX:Condition=1],((Cyp.long:4[&&NHX:Condition=1],Cyp.rotu:5[&&NHX:Condition=1]):9[&&NHX:Condition=1],(Cyp.papy:13[&&NHX:Condition=1],Cyp.ustu:8[&&NHX:Condition=1]):4[&&NHX:Condition=1]):1[&&NHX:Condition=1]):4[&&NHX:Condition=1]):31[&&NHX:Condition=1]):2[&&NHX:Condition=1]):5[&&NHX:Condition=1]):1[&&NHX:Condition=1]):48[&&NHX:Condition=1:Transition=1]):4[&&NHX:Condition=0]):1[&&NHX:Condition=0]):5[&&NHX:Condition=0]):5[&&NHX:Condition=0]):12[&&NHX:Condition=0]):29[&&NHX:Condition=0]):4[&&NHX:Condition=0]):3[&&NHX:Condition=0]):9[&&NHX:Condition=0]):4[&&NHX:Condition=0],(Blysmus:31[&&NHX:Condition=0],((Eriophor:5[&&NHX:Condition=0],Scirpus:9[&&NHX:Condition=0]):14[&&NHX:Condition=0],(((Schoenox:22[&&NHX:Condition=0],Uncin.un:18[&&NHX:Condition=0]):15[&&NHX:Condition=0],Uncin.ph:25[&&NHX:Condition=0]):11[&&NHX:Condition=0],(Carex.com:4[&&NHX:Condition=0],(Carex.hal:10[&&NHX:Condition=0],(Carex.ber:8[&&NHX:Condition=0],Carex.pen:3[&&NHX:Condition=0]):3[&&NHX:Condition=0]):1[&&NHX:Condition=0]):20[&&NHX:Condition=0]):37[&&NHX:Condition=0]):15[&&NHX:Condition=0]):21[&&NHX:Condition=0]):5[&&NHX:Condition=0],((Rhy.alba:26[&&NHX:Condition=0],Rhy.grac:45[&&NHX:Condition=0]):21[&&NHX:Condition=0],(Rhy.albi:11[&&NHX:Condition=0],(Rhy.rubr:8[&&NHX:Condition=1],(Rhy.glob:32[&&NHX:Condition=1],Rhy.glo2:19[&&NHX:Condition=1]):11[&&NHX:Condition=1]):31[&&NHX:Condition=1:Transition=1]):59[&&NHX:Condition=0]):17[&&NHX:Condition=0]):20[&&NHX:Condition=0],Carpha:79[&&NHX:Condition=0]):8[&&NHX:Condition=0],(Schoenus:64[&&NHX:Condition=0],(Baumea:18[&&NHX:Condition=0],Machaeri:5[&&NHX:Condition=0]):43[&&NHX:Condition=0]):32[&&NHX:Condition=0]):16[&&NHX:Condition=0],Cladium:78[&&NHX:Condition=0]):13[&&NHX:Condition=0],(Coleochlo:111[&&NHX:Condition=0],Microdra:57[&&NHX:Condition=0]):80[&&NHX:Condition=0]):38[&&NHX:Condition=0])[&&NHX:Condition=0];" in
  test_string_equal data (to_string (from_string_exn data))

let%expect_test "positive exponent in float" =
  let data = "(Zapus_hudsonius:0.713637[&&NHX:BranchLength=8.781397e-01:BranchLength_max=1.076536e+00:BranchLength_min=7.849905e-01]);" in
  print_endline @@ unparse (from_string_exn data) ;
  [%expect {| (Zapus_hudsonius:0.713637[&&NHX:BranchLength=8.781397e-01:BranchLength_max=1.076536e+00:BranchLength_min=7.849905e-01]) |}]

module Tree_repr = struct
  type ast = t

  type node_info = {
    name : string option ;
    tags : tag list ;
  }

  type tree = (node_info, node_info, float option) Tree.t
  type branch = (node_info, node_info, float option) Tree.branch

  type t =
    | Tree of tree
    | Branch of branch

  let of_ast newick =
    let rec node ({name ; tags ; children ; _} : ast) =
      match children with
      | [] ->
        Tree.leaf { name ; tags }
      | children ->
        let branches = List.map children ~f:branch in
        Tree.node { name ; tags } (List1.of_list_exn branches)
    and branch ({ parent_branch ; _ } as tree : ast) =
      let tip = node tree in
      Tree.branch parent_branch tip
    in
    match newick.parent_branch with
    | None -> Tree (node newick)
    | Some _ -> Branch (branch newick)

  let ast_of_tree ?parent_branch (tree_repr : tree) =
    of_tree tree_repr
      ?parent_branch
      ~node_id:(fun { name ; _} -> name)
      ~node_tags:(fun { tags ; _ } -> tags)
      ~leaf_id:(fun { name ; _} -> name)
      ~leaf_tags:(fun { tags ; _ } -> tags)
      ~branch_length:(Fn.id)

  let to_ast = function
    | Tree t -> ast_of_tree t
    | Branch (Tree.Branch b) -> ast_of_tree ?parent_branch:b.data b.tip

  let map_inner_tree tree ~f =
    match tree with
    | Tree t -> Tree (f t)
    | Branch (Branch { tip ; data }) ->
      Branch (Tree.branch data (f tip))

  let with_inner_tree tree ~f =
    match tree with
    | Tree t
    | Branch (Branch { tip = t ; _ }) -> f t
end
