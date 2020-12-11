open Batteries

let () =
  let file_name = Sys.argv.(1) in
  let open Michelson in
  let file = open_in file_name in
  let mich_adt = Parser.start Lexer.next_token (Lexing.from_channel file) in
  let open Tezcheck in
  let tezla_cfg = Dataflow.Cfg.generate_from_program mich_adt in
  let module S = Tezcheck.Sign_analysis.Solve (struct
    let graph = tezla_cfg
  end) in
  Set.iter
    (fun i ->
      Printf.printf "%d: %s\n" i (S.result_to_string (S.get_exit_result i)))
    (Dataflow.Cfg.labels tezla_cfg)
