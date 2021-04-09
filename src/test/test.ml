open Core_kernel
open Graphlib.Std
open Tezla_cfg
module Interval = Domains.Interval_ext_domain
module Iter = Absinterp.Make (Interval)

let () =
  let filename = "../../../../tests/factorial.tz" in
  let michelson = Michelson.Carthage.Parser.parse_file filename in
  let michelson = Michelson.Carthage.Parser.convert filename michelson in
  let ((param, storage, code) as prog) =
    Tezla.Converter.convert_program (ref (-1)) michelson
  in
  let cfg = Flow_graph.generate_from_program prog in
  let () = Flow_graph.dot_output cfg "cfg.dot" in
  let solution = Iter.solution cfg param storage in
  let final = Set.choose_exn cfg.finals in
  let r = Solution.get solution final in
  print_endline [%string "%{final#Cfg_node}: %{r#Interval}"]
