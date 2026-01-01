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
let build_dir = "build"
let emit_dir = Filename.concat build_dir "emit"
let debug_dir = Filename.concat build_dir "debug"

let ensure_dir dir =
  if not (Sys.file_exists dir && Sys.is_directory dir) then Unix.mkdir dir 0o755
;;

let print_error ?(file = None) msg =
  match file with
  | Some f -> Printf.eprintf "%s: error: %s\n" f msg
  | None -> Printf.eprintf "error: %s\n" msg
;;

let has_parse_errors nodes =
  List.exists
    (function
      | Ast.Error _ -> true
      | _ -> false)
    nodes
;;

let report_parse_errors file nodes =
  List.iter
    (function
      | Ast.Error msg -> print_error ~file:(Some file) msg
      | _ -> ())
    nodes
;;

let rec get_stdlib_files_recursive dir prefix =
  let files = Sys.readdir dir |> Array.to_list in
  let nova_files =
    List.filter (fun f -> Filename.check_suffix f ".nova") files
    |> List.map (fun f ->
      let full_path = Filename.concat dir f in
      let module_name =
        if prefix = ""
        then Filename.remove_extension f
        else prefix ^ "." ^ Filename.remove_extension f
      in
      full_path, module_name)
  in
  let subdirs =
    List.filter
      (fun f ->
         let full_path = Filename.concat dir f in
         try Sys.is_directory full_path with
         | _ -> false)
      files
    |> List.concat_map (fun f ->
      let full_path = Filename.concat dir f in
      let new_prefix = if prefix = "" then f else prefix ^ "." ^ f in
      get_stdlib_files_recursive full_path new_prefix)
  in
  nova_files @ subdirs
;;

let get_stdlib_files () = get_stdlib_files_recursive stdlib_dir ""

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

let format_analysis_error = function
  | Analysis.Error (Analysis.Undefined_variable (v, sugg)) ->
    let hint =
      match sugg with
      | [] -> ""
      | xs -> " (did you mean: " ^ String.concat ", " xs ^ ")"
    in
    "Undefined variable: " ^ v ^ hint
  | Analysis.Error (Analysis.Type_mismatch (t1, t2)) ->
    Printf.sprintf
      "Type mismatch: expected %s, got %s"
      (Ast.show_typ t1)
      (Ast.show_typ t2)
  | Analysis.Error (Analysis.Duplicate_declaration (v, (r, c))) ->
    Printf.sprintf "Duplicate declaration: %s at %d:%d" v r c
  | Analysis.Error (Analysis.Invalid_operation msg) -> "Invalid operation: " ^ msg
  | Analysis.Error (Analysis.Missing_return_type f) ->
    "Missing return type for function: " ^ f
  | Analysis.Warning _ -> "Unexpected warning in error list"
;;

let run_cmd command =
  let code = Sys.command command in
  if code <> 0 then Printf.eprintf "Command failed (%d): %s\n" code command;
  code
;;

let process_parse stdlib_flag files =
  let std_lib_files, user_files = get_files_for_backend stdlib_flag files in
  let all_files =
    List.map fst std_lib_files @ if user_files = [] then [] else user_files
  in
  List.iter
    (fun file ->
       let tokens = Lexer.lex_from_file file in
       let nodes = Parser.parse (Parser.create tokens) in
       if has_parse_errors nodes
       then (
         report_parse_errors file nodes;
         Printf.eprintf "Parse failed for %s\n" file)
       else Printf.printf "parse %s ok\n" (Filename.basename file))
    all_files
;;

let process_codegen stdlib_flag files =
  ensure_dir build_dir;
  ensure_dir emit_dir;
  let std_lib_files, user_files = get_files_for_backend stdlib_flag files in
  let all_files =
    List.map fst std_lib_files @ if user_files = [] then [] else user_files
  in
  List.iter
    (fun file ->
       (* Printf.printf "Generating code for: %s\n" file; *)
       let tokens = Lexer.lex_from_file file in
       let nodes = Parser.parse (Parser.create tokens) in
       if has_parse_errors nodes
       then (
         report_parse_errors file nodes;
         Printf.eprintf "Skipping code generation due to parse errors in %s\n" file)
       else (
         let nodes = Preprocessor.preprocess nodes in
         match Codegen.generate_code nodes with
         | Ok asm ->
           let base = Filename.remove_extension (Filename.basename file) in
           let output = Filename.concat emit_dir (base ^ ".asm") in
           let oc = open_out output in
           output_string oc asm;
           close_out oc;
           Printf.printf "  Wrote %s\n" output;
           Printf.printf "===== %s (generated) =====\n%s\n" output asm
         | Error err ->
           Printf.eprintf
             "  Code generation error in %s: %s\n"
             file
             (format_analysis_error (Analysis.Error err))))
    all_files
;;

let compile_to_exe stdlib_flag files exe_file =
  ensure_dir build_dir;
  ensure_dir emit_dir;
  ensure_dir debug_dir;
  let std_lib_files, user_files = get_files_for_backend stdlib_flag files in
  let all_files = List.map fst std_lib_files @ user_files in
  if all_files = []
  then (
    Printf.eprintf "Error: No input files provided.\n";
    1)
  else (
    let assembler_ok = Sys.command "command -v nasm >/dev/null 2>&1" = 0 in
    if not assembler_ok
    then (
      Printf.eprintf
        "Error: nasm is required for compilation but was not found in PATH.\n";
      1)
    else (
      let failed = ref false in
      let asm_files = ref [] in
      List.iter
        (fun file ->
           if not !failed
           then (
             (* Printf.printf "Generating code for: %s\n" file; *)
             let tokens = Lexer.lex_from_file file in
             let nodes = Parser.parse (Parser.create tokens) in
             if has_parse_errors nodes
             then (
               report_parse_errors file nodes;
               Printf.eprintf "Skipping compilation due to parse errors in %s\n" file;
               failed := true)
             else (
               let nodes = Preprocessor.preprocess nodes in
               match Codegen.generate_code nodes with
               | Error err ->
                 Printf.eprintf
                   "Compilation halted: %s\n"
                   (format_analysis_error (Analysis.Error err));
                 failed := true
               | Ok asm ->
                 let base = Filename.remove_extension (Filename.basename file) in
                 let asm_path = Filename.concat emit_dir (base ^ ".asm") in
                 let oc = open_out asm_path in
                 output_string oc asm;
                 close_out oc;
                 asm_files := asm_path :: !asm_files)))
        all_files;
      if !failed
      then 1
      else (
        let asm_files = List.rev !asm_files in
        let obj_files =
          List.map
            (fun asm_path ->
               Filename.concat
                 emit_dir
                 (Filename.basename (Filename.remove_extension asm_path) ^ ".o"))
            asm_files
        in
        let assemble asm_path obj_path =
          let cmd =
            Printf.sprintf
              "nasm -felf64 -o %s %s"
              (Filename.quote obj_path)
              (Filename.quote asm_path)
          in
          run_cmd cmd
        in
        let rec assemble_all = function
          | [], [] -> 0
          | asm :: asms, obj :: objs ->
            let code = assemble asm obj in
            if code <> 0 then code else assemble_all (asms, objs)
          | _ -> 1
        in
        let asm_result = assemble_all (asm_files, obj_files) in
        if asm_result <> 0
        then asm_result
        else (
          let objs = String.concat " " (List.map Filename.quote obj_files) in
          let link_cmd =
            Printf.sprintf "cc -no-pie -o %s %s" (Filename.quote exe_file) objs
          in
          let link_code = run_cmd link_cmd in
          List.iter
            (fun obj ->
               try Sys.remove obj with
               | _ -> ())
            obj_files;
          link_code))))
;;

let process_compile stdlib_flag files =
  let exe_file = Filename.concat debug_dir "main" in
  let exit_code = compile_to_exe stdlib_flag files exe_file in
  if exit_code <> 0 then exit exit_code
;;

let process_run stdlib_flag files =
  let exe_file = Filename.concat debug_dir "main" in
  let exit_code = compile_to_exe stdlib_flag files exe_file in
  if exit_code <> 0 then exit exit_code;
  let run_cmd =
    if Filename.is_relative exe_file then Printf.sprintf "./%s" exe_file else exe_file
  in
  let run_exit_code = Sys.command run_cmd in
  if run_exit_code <> 0 then exit run_exit_code
;;

let process_clean () =
  let _ = Sys.command (Printf.sprintf "rm -rf %s/* %s/*" debug_dir emit_dir) in
  ()
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

let clean_cmd =
  let doc = "Clean build artifacts." in
  let info = Cmd.info "clean" ~doc in
  Cmd.v info Term.(const process_clean $ const ())
;;

let main_cmd =
  let doc = "Supernova compiler." in
  let info = Cmd.info "novac" ~doc in
  Cmd.group info [ parse_cmd; codegen_cmd; compile_cmd; run_cmd; clean_cmd ]
;;
