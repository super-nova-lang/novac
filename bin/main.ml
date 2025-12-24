open Novac
open Cmdliner

let process_lex files =
  List.iter
    (fun file ->
       Printf.printf "File: %s\n" file;
       let tokens = Lexer.lex_from_file file in
       List.iter (fun (t, _) -> Printf.printf "  found: %s\n" (Token.show t)) tokens)
    files
;;

let process_parse files =
  List.iter
    (fun file ->
       Printf.printf "File: %s\n" file;
       let tokens = Lexer.lex_from_file file in
       let nodes = Parser.parse (Parser.create tokens) in
       List.iter (fun n -> Printf.printf "  found: %s\n" (Ast.show n)) nodes)
    files
;;

let process_codegen files =
  List.iter
    (fun file ->
       let module_name = Filename.remove_extension (Filename.basename file) in
       Codegen.set_module_name module_name;
       let tokens = Lexer.lex_from_file file in
       let nodes = Parser.parse (Parser.create tokens) in
       List.iter Codegen.codegen nodes)
    files;
  Codegen.finish_module ();
  Llvm.dump_module Codegen.the_module
;;

let compile_to_exe files exe_file =
  List.iter
    (fun file ->
       let module_name = Filename.remove_extension (Filename.basename file) in
       Codegen.set_module_name module_name;
       let tokens = Lexer.lex_from_file file in
       let nodes = Parser.parse (Parser.create tokens) in
       List.iter Codegen.codegen nodes)
    files;
  Codegen.finish_module ();
  let ll_code = Llvm.string_of_llmodule Codegen.the_module in
  let ll_file = Filename.temp_file "output" ".ll" in
  let oc = open_out ll_file in
  output_string oc ll_code;
  close_out oc;
  let cmd = Printf.sprintf "clang %s -o %s -Wno-override-module" ll_file exe_file in
  let exit_code = Sys.command cmd in
  Sys.remove ll_file;
  exit_code
;;

let process_compile files =
  let exe_file = "main" in
  let exit_code = compile_to_exe files exe_file in
  if exit_code <> 0 then exit exit_code
;;

let process_run files =
  let exe_file = Filename.temp_file "nova_run" ".exe" in
  let exit_code = compile_to_exe files exe_file in
  if exit_code <> 0
  then (
    Sys.remove exe_file;
    exit exit_code);
  let run_cmd =
    if Filename.is_relative exe_file then Printf.sprintf "./%s" exe_file else exe_file
  in
  let run_exit_code = Sys.command run_cmd in
  Sys.remove exe_file;
  if run_exit_code <> 0 then exit run_exit_code
;;

let files =
  let doc = "The files to process." in
  Arg.(non_empty & pos_all string [] & info [] ~docv:"FILES" ~doc)
;;

let lex_cmd =
  let doc = "Lex the input files." in
  let info = Cmd.info "lex" ~doc in
  Cmd.v info Term.(const process_lex $ files)
;;

let parse_cmd =
  let doc = "Parse the input files." in
  let info = Cmd.info "parse" ~doc in
  Cmd.v info Term.(const process_parse $ files)
;;

let codegen_cmd =
  let doc = "Generate code for the input files." in
  let info = Cmd.info "codegen" ~doc in
  Cmd.v info Term.(const process_codegen $ files)
;;

let compile_cmd =
  let doc = "Compile the input files to an executable." in
  let info = Cmd.info "compile" ~doc in
  Cmd.v info Term.(const process_compile $ files)
;;

let run_cmd =
  let doc = "Compile and run the input files." in
  let info = Cmd.info "run" ~doc in
  Cmd.v info Term.(const process_run $ files)
;;

let main_cmd =
  let doc = "Supernova compiler." in
  let info = Cmd.info "novac" ~doc in
  Cmd.group info [ lex_cmd; parse_cmd; codegen_cmd; compile_cmd; run_cmd ]
;;

let () = exit (Cmd.eval main_cmd)
