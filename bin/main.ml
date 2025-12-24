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
  Codegen.finish_module ();
  Llvm.dump_module Codegen.the_module
;;

let process_compile file =
  let tokens = Lexer.lex_from_file file in
  let nodes = Parser.parse (Parser.create tokens) in
  List.iter Codegen.codegen nodes;
  Codegen.finish_module ();
  let ll_code = Llvm.string_of_llmodule Codegen.the_module in
  let ll_file = Filename.temp_file "output" ".ll" in
  let oc = open_out ll_file in
  output_string oc ll_code;
  close_out oc;
  let exe_file = "main" in
  let cmd = Printf.sprintf "clang %s -o %s -Wno-override-module" ll_file exe_file in
  let exit_code = Sys.command cmd in
  Sys.remove ll_file;
  if exit_code <> 0 then exit exit_code
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

let compile_cmd =
  let doc = "Compile the input file to an executable." in
  let info = Cmd.info "compile" ~doc in
  Cmd.v info Term.(const process_compile $ file)
;;

let main_cmd =
  let doc = "Supernova compiler." in
  let info = Cmd.info "novac" ~doc in
  Cmd.group info [ lex_cmd; parse_cmd; codegen_cmd; compile_cmd ]
;;

let () = exit (Cmd.eval main_cmd)
