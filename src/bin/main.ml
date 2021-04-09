module Cfg = Tezla_cfg.Flow_graph
module Cfg_node = Tezla_cfg.Cfg_node

let () =
  let file_name = Sys.argv.(1) in
  let open Michelson.Carthage in
  let tokens = Parser.parse_file file_name in
  let mich_adt = Parser.convert tokens in
  let tezla_adt = Tezla.Converter.convert_program (ref (-1)) mich_adt in
  let open Tezcheck in
  let tezla_cfg = Framework.Dataflow.Cfg.generate_from_program tezla_adt in
  let module S = Tezcheck.Analysis.Sign_analysis.Solve (struct
    let graph = tezla_cfg
  end) in
  List.iter
    (fun n ->
      Printf.printf "%d: %s\n" n.Cfg_node.label
        (S.result_to_string
           (Graphlib.Std.Solution.get S.solution
              { node = n.label; node_label = n })))
    (Framework.Dataflow.Cfg.get_blocks tezla_cfg)
