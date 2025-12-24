open Novac
open Cmdliner

let process_lex file =
  let tokens = Lexer.lex_from_file file in
  List.iter (fun (t, _) -> Printf.printf "%s\n" (Token.show t)) tokens
;;

let process_parse file =
  let tokens = Lexer.lex_from_file file in
  let nodes = Parser.parse (Parser.create tokens) in
  List.iter (fun n -> Printf.printf "%s\n" (Ast.show n)) nodes
;;

let process_codegen file =
  let tokens = Lexer.lex_from_file file in
  let nodes = Parser.parse (Parser.create tokens) in
  List.iter Codegen.codegen nodes;
  Llvm.dump_module Codegen.the_module
;;

let file =
  let doc = "The file to process." in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"FILE" ~doc)
;;

let lex_cmd =
  let doc = "Lex the input file." in
  let info = Cmd.info "lex" ~doc in
  Cmd.v info Term.(const process_lex $ file)
;;

let parse_cmd =
  let doc = "Parse the input file." in
  let info = Cmd.info "parse" ~doc in
  Cmd.v info Term.(const process_parse $ file)
;;

let codegen_cmd =
  let doc = "Generate code for the input file." in
  let info = Cmd.info "codegen" ~doc in
  Cmd.v info Term.(const process_codegen $ file)
;;

let main_cmd =
  let doc = "Supernova compiler." in
  let info = Cmd.info "novac" ~doc in
  Cmd.group info [ lex_cmd; parse_cmd; codegen_cmd ]
;;

let () = exit (Cmd.eval main_cmd)
