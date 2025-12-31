open Cmdliner

let stdlib_flag =
  let doc =
    "Use the standard library files from the 'stdlib' directory instead of providing \
     files."
  in
  Arg.(value & flag & info [ "s"; "stdlib" ] ~doc)
;;

let files_arg =
  let doc = "Paths to the .nova files." in
  Arg.(value & pos_all file [] & info [] ~docv:"FILES" ~doc)
;;

let stdlib_dir = "stdlib"

let get_stdlib_files () =
  let files = Sys.readdir stdlib_dir |> Array.to_list in
  List.filter (fun f -> Filename.check_suffix f ".nova") files
  |> List.map (fun f -> Filename.concat stdlib_dir f)
;;

let get_files_for_frontend stdlib_flag files =
  if stdlib_flag
  then get_stdlib_files ()
  else if files = []
  then (
    Printf.eprintf "Error: No input files provided.\n";
    exit 1)
  else files
;;

let get_files_for_backend stdlib_flag provided_files =
  let std_lib_files = get_stdlib_files () in
  if stdlib_flag
  then
    std_lib_files, [] (* If --stdlib is passed, only process stdlib files, no app files *)
  else (
    match provided_files with
    | [] ->
      (* No user files provided *)
      std_lib_files, []
      (* Process only stdlib files *)
    | _ ->
      (* User files provided *)
      std_lib_files, provided_files)
;;

(* Process stdlib files PLUS user files *)

let process_lex stdlib_flag files =
  let files_to_process = get_files_for_frontend stdlib_flag files in
  List.iter
    (fun file ->
       Printf.printf "File: %s\n" file;
       let tokens = Lexer.lex_from_file file in
       List.iter (fun (t, _) -> Printf.printf "  found: %s\n" (Token.show t)) tokens)
    files_to_process
;;

let process_parse stdlib_flag files =
  let files_to_process = get_files_for_frontend stdlib_flag files in
  List.iter
    (fun file ->
       Printf.printf "File: %s\n" file;
       let tokens = Lexer.lex_from_file file in
       let nodes = Parser.parse (Parser.create tokens) in
       List.iter (fun n -> Printf.printf "  found: %s\n" (Ast.show n)) nodes)
    files_to_process
;;

let process_codegen stdlib_flag files =
  let std_files, app_files = get_files_for_backend stdlib_flag files in
  let process_file file is_stdlib =
    Codegen.reset_module ();
    let base_module_name = Filename.remove_extension (Filename.basename file) in
    let module_name =
      if is_stdlib
      then "std_" ^ base_module_name
      else base_module_name
    in
    Codegen.set_module_name module_name;
    let tokens = Lexer.lex_from_file file in
    let nodes = Parser.parse (Parser.create tokens) in
    (* Run analysis to get type information *)
    let errors, warnings = Analysis.analyze nodes in
    (* Report analysis results *)
    if errors <> [] then (
      Printf.printf "Analysis errors in %s:\n" file;
      List.iter (fun err -> Printf.printf "  %s\n" (match err with
        | Analysis.Error (Analysis.Undefined_variable v) -> "Undefined variable: " ^ v
        | Analysis.Error (Analysis.Type_mismatch (t1, t2)) -> 
          Printf.sprintf "Type mismatch: expected %s, got %s" (Ast.show_typ t1) (Ast.show_typ t2)
        | Analysis.Error (Analysis.Duplicate_declaration v) -> "Duplicate declaration: " ^ v
        | Analysis.Error (Analysis.Invalid_operation msg) -> "Invalid operation: " ^ msg
        | Analysis.Error (Analysis.Missing_return_type f) -> "Missing return type for function: " ^ f
        | Analysis.Warning _ -> "Unexpected warning in error list"
      )) errors
    );
    if warnings <> [] then (
      Printf.printf "Analysis warnings in %s:\n" file;
      List.iter (fun warn -> Printf.printf "  %s\n" (match warn with
        | Analysis.Warning (Analysis.Unused_variable v) -> "Unused variable: " ^ v
        | Analysis.Warning (Analysis.Shadowed_variable v) -> "Shadowed variable: " ^ v
        | Analysis.Error _ -> "Unexpected error in warning list"
      )) warnings
    );
    (* Only proceed with codegen if no errors *)
    if errors = [] then (
      (* Set analysis context for codegen *)
      Codegen.set_analysis_nodes nodes;
      List.iter Codegen.codegen nodes;
      Codegen.finish_module ();
      Printf.printf "Module for %s:\n" file;
      Llvm.dump_module !Codegen.the_module
    ) else
      Printf.printf "Skipping codegen for %s due to analysis errors\n" file
  in
  List.iter (fun file -> process_file file true) std_files;
  List.iter (fun file -> process_file file false) app_files
;;

let compile_to_exe stdlib_flag files exe_file =
  let std_files, app_files = get_files_for_backend stdlib_flag files in
  let compile_single_file file is_stdlib =
    Codegen.reset_module ();
    let base_module_name = Filename.remove_extension (Filename.basename file) in
    let module_name =
      if is_stdlib
      then "std_" ^ base_module_name
      else base_module_name
    in
    Codegen.set_module_name module_name;
    let tokens = Lexer.lex_from_file file in
    let nodes = Parser.parse (Parser.create tokens) in
    List.iter Codegen.codegen nodes;
    Codegen.finish_module ();
    let ll_code = Llvm.string_of_llmodule !Codegen.the_module in
    let ll_file = Filename.temp_file (Filename.basename file) ".ll" in
    let oc = open_out ll_file in
    output_string oc ll_code;
    close_out oc;
    ll_file
  in
  let std_ll_files = List.map (fun file -> compile_single_file file true) std_files in
  let app_ll_files = List.map (fun file -> compile_single_file file false) app_files in
  let all_ll_files = std_ll_files @ app_ll_files in
  let ll_files_str = String.concat " " all_ll_files in
  let cmd = Printf.sprintf "clang %s -o %s -Wno-override-module" ll_files_str exe_file in
  let exit_code = Sys.command cmd in
  List.iter Sys.remove all_ll_files;
  exit_code
;;

let process_compile stdlib_flag files =
  let exe_file = "main" in
  let exit_code = compile_to_exe stdlib_flag files exe_file in
  if exit_code <> 0 then exit exit_code
;;

let process_run stdlib_flag files =
  let exe_file = Filename.temp_file "nova_run" ".exe" in
  let exit_code = compile_to_exe stdlib_flag files exe_file in
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

let process_analyze stdlib_flag files =
  let files_to_process = get_files_for_frontend stdlib_flag files in
  List.iter
    (fun file ->
       Printf.printf "Analyzing: %s\n" file;
       let tokens = Lexer.lex_from_file file in
       let nodes = Parser.parse (Parser.create tokens) in
       let errors, warnings = Analysis.analyze nodes in
       if errors <> [] then (
         Printf.printf "Errors:\n";
         List.iter (fun err -> Printf.printf "  %s\n" (match err with
           | Analysis.Error (Analysis.Undefined_variable v) -> "Undefined variable: " ^ v
           | Analysis.Error (Analysis.Type_mismatch (t1, t2)) -> 
             Printf.sprintf "Type mismatch: expected %s, got %s" (Ast.show_typ t1) (Ast.show_typ t2)
           | Analysis.Error (Analysis.Duplicate_declaration v) -> "Duplicate declaration: " ^ v
           | Analysis.Error (Analysis.Invalid_operation msg) -> "Invalid operation: " ^ msg
           | Analysis.Error (Analysis.Missing_return_type f) -> "Missing return type for function: " ^ f
           | Analysis.Warning _ -> "Unexpected warning in error list"
         )) errors
       );
       if warnings <> [] then (
         Printf.printf "Warnings:\n";
         List.iter (fun warn -> Printf.printf "  %s\n" (match warn with
           | Analysis.Warning (Analysis.Unused_variable v) -> "Unused variable: " ^ v
           | Analysis.Warning (Analysis.Shadowed_variable v) -> "Shadowed variable: " ^ v
           | Analysis.Error _ -> "Unexpected error in warning list"
         )) warnings
       );
       if errors = [] && warnings = [] then
         Printf.printf "  No issues found.\n")
    files_to_process
;;

let lex_cmd =
  let doc = "Lex the input files." in
  let info = Cmd.info "lex" ~doc in
  Cmd.v info Term.(const process_lex $ stdlib_flag $ files_arg)
;;

let parse_cmd =
  let doc = "Parse the input files." in
  let info = Cmd.info "parse" ~doc in
  Cmd.v info Term.(const process_parse $ stdlib_flag $ files_arg)
;;

let codegen_cmd =
  let doc = "Generate code for the input files." in
  let info = Cmd.info "codegen" ~doc in
  Cmd.v info Term.(const process_codegen $ stdlib_flag $ files_arg)
;;

let compile_cmd =
  let doc = "Compile the input files to an executable." in
  let info = Cmd.info "compile" ~doc in
  Cmd.v info Term.(const process_compile $ stdlib_flag $ files_arg)
;;

let run_cmd =
  let doc = "Compile and run the input files." in
  let info = Cmd.info "run" ~doc in
  Cmd.v info Term.(const process_run $ stdlib_flag $ files_arg)
;;

let analyze_cmd =
  let doc = "Perform static analysis on the input files." in
  let info = Cmd.info "analyze" ~doc in
  Cmd.v info Term.(const process_analyze $ stdlib_flag $ files_arg)
;;

let main_cmd =
  let doc = "Supernova compiler." in
  let info = Cmd.info "novac" ~doc in
  Cmd.group info [ lex_cmd; parse_cmd; analyze_cmd; codegen_cmd; compile_cmd; run_cmd ]
;;
