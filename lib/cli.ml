open Cmdliner
module Json = Yojson.Safe
module String_map = Map.Make (String)

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
let tests_dir = "tests"
let expected_file = Filename.concat tests_dir ".expected.json"

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

let all_files_for_backend stdlib_flag provided_files =
  let std_lib_files, user_files = get_files_for_backend stdlib_flag provided_files in
  List.map fst std_lib_files @ if user_files = [] then [] else user_files
;;

let ast_to_string nodes = nodes |> List.map Ast.show |> String.concat "\n"

let read_all_from_channel ic =
  let buf = Buffer.create 256 in
  let chunk = Bytes.create 4096 in
  let rec loop () =
    match input ic chunk 0 (Bytes.length chunk) with
    | 0 -> ()
    | n ->
      Buffer.add_subbytes buf chunk 0 n;
      loop ()
  in
  loop ();
  Buffer.contents buf
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

let parse_and_validate file =
  let tokens = Lexer.lex_from_file file in
  let nodes = Parser.parse (Parser.create tokens) in
  if has_parse_errors nodes
  then (
    report_parse_errors file nodes;
    let msg =
      nodes
      |> List.find_map (function
        | Ast.Error m -> Some m
        | _ -> None)
      |> Option.value ~default:"Parse failed"
    in
    Utils.raise_error ~phase:Utils.Parser ~file ~row:0 ~col:0 msg)
  else nodes
;;

let generate_asm_for_file file =
  let nodes = parse_and_validate file in
  let nodes = Preprocessor.preprocess nodes in
  match Codegen.generate_code nodes with
  | Ok asm -> asm
  | Error err ->
    Utils.raise_error
      ~phase:Utils.Codegen
      ~file
      ~row:0
      ~col:0
      (format_analysis_error (Analysis.Error err))
;;

let string_of_parse stdlib_flag files =
  let buf = Buffer.create 256 in
  all_files_for_backend stdlib_flag files
  |> List.iteri (fun idx file ->
    let nodes = parse_and_validate file in
    if idx > 0 then Buffer.add_char buf '\n';
    Buffer.add_string buf (ast_to_string nodes));
  Buffer.contents buf
;;

let string_of_codegen stdlib_flag files =
  ensure_dir build_dir;
  ensure_dir emit_dir;
  let buf = Buffer.create 256 in
  all_files_for_backend stdlib_flag files
  |> List.iteri (fun idx file ->
    let asm = generate_asm_for_file file in
    let base = Filename.remove_extension (Filename.basename file) in
    let output = Filename.concat emit_dir (base ^ ".asm") in
    let oc = open_out output in
    output_string oc asm;
    close_out oc;
    if idx > 0 then Buffer.add_char buf '\n';
    Buffer.add_string buf (Printf.sprintf "  Wrote %s\n" output);
    Buffer.add_string buf (Printf.sprintf "===== %s (generated) =====\n%s\n" output asm));
  Buffer.contents buf |> String.trim
;;

let run_cmd command =
  let code = Sys.command command in
  if code <> 0 then Printf.eprintf "Command failed (%d): %s\n" code command;
  code
;;

let process_parse stdlib_flag files =
  let output = string_of_parse stdlib_flag files in
  if String.length output > 0 then Printf.printf "%s\n" output
;;

let process_codegen stdlib_flag files =
  let output = string_of_codegen stdlib_flag files in
  if String.length output > 0 then Printf.printf "%s\n" output
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

type run_status =
  | Run_ok of int * string
  | Run_error of Utils.novac_phase * int * string

let run_with_status stdlib_flag files =
  let exe_file =
    match files with
    | [ file ] ->
      let base = Filename.remove_extension (Filename.basename file) in
      Filename.concat debug_dir base
    | _ -> Filename.concat debug_dir "main"
  in
  let exit_code = compile_to_exe stdlib_flag files exe_file in
  if exit_code <> 0
  then Run_error (Utils.Codegen, exit_code, "Compilation failed")
  else (
    let run_cmd =
      if Filename.is_relative exe_file then Printf.sprintf "./%s" exe_file else exe_file
    in
    let ic = Unix.open_process_in run_cmd in
    let output = read_all_from_channel ic in
    match Unix.close_process_in ic with
    | Unix.WEXITED code -> Run_ok (code, String.trim output)
    | Unix.WSIGNALED signal ->
      Run_error
        (Utils.Io, 128 + signal, Printf.sprintf "Program stopped by signal %d" signal)
    | Unix.WSTOPPED signal ->
      Run_error
        (Utils.Io, 128 + signal, Printf.sprintf "Program stopped by signal %d" signal))
;;

let string_of_run stdlib_flag files =
  match run_with_status stdlib_flag files with
  | Run_ok (0, output) -> output
  | Run_ok (code, _) ->
    Utils.raise_error
      ~phase:Utils.Io
      ~file:"<run>"
      ~row:0
      ~col:0
      (Printf.sprintf "Program exited with code %d" code)
  | Run_error (Utils.Codegen, _, msg) ->
    Utils.raise_error ~phase:Utils.Codegen ~file:"<compile>" ~row:0 ~col:0 msg
  | Run_error (phase, _, msg) -> Utils.raise_error ~phase ~file:"<run>" ~row:0 ~col:0 msg
;;

let process_compile stdlib_flag files =
  let exe_file = Filename.concat debug_dir "main" in
  let exit_code = compile_to_exe stdlib_flag files exe_file in
  if exit_code <> 0 then exit exit_code
;;

let process_run stdlib_flag files =
  let output = string_of_run stdlib_flag files in
  if String.length output > 0 then Printf.printf "%s\n" output
;;

let process_clean () =
  let _ = Sys.command (Printf.sprintf "rm -rf %s/* %s/*" debug_dir emit_dir) in
  ()
;;

type test_snapshot =
  { file : string
  ; parse : string
  ; codegen : string
  ; run : string
  ; code : int
  }

let capture_output thunk =
  try Ok (thunk ()) with
  | Utils.Novac_error (_, err) ->
    Error (Printf.sprintf "%s:%d:%d: %s" err.file err.row err.col err.msg)
  | exn -> Error (Printexc.to_string exn)
;;

let string_of_result = function
  | Ok s -> s
  | Error msg -> "ERROR: " ^ msg
;;

let test_snapshot_to_json t =
  `Assoc
    [ "file", `String t.file
    ; "parse", `String t.parse
    ; "codegen", `String t.codegen
    ; "run", `String t.run
    ; "code", `Int t.code
    ]
;;

let test_snapshot_of_json json =
  let expect_string key props =
    match List.assoc_opt key props with
    | Some (`String v) -> v
    | _ ->
      Utils.raise_error
        ~phase:Utils.Io
        ~file:expected_file
        ~row:0
        ~col:0
        (Printf.sprintf "Missing or invalid '%s' entry" key)
  in
  let expect_int key props =
    match List.assoc_opt key props with
    | Some (`Int v) -> v
    | _ ->
      Utils.raise_error
        ~phase:Utils.Io
        ~file:expected_file
        ~row:0
        ~col:0
        (Printf.sprintf "Missing or invalid '%s' entry" key)
  in
  match json with
  | `Assoc props ->
    { file = expect_string "file" props
    ; parse = expect_string "parse" props
    ; codegen = expect_string "codegen" props
    ; run = expect_string "run" props
    ; code = expect_int "code" props
    }
  | _ ->
    Utils.raise_error
      ~phase:Utils.Io
      ~file:expected_file
      ~row:0
      ~col:0
      "Expected JSON object for snapshot"
;;

let nova_test_files () =
  if Sys.file_exists tests_dir && Sys.is_directory tests_dir
  then
    Sys.readdir tests_dir
    |> Array.to_list
    |> List.filter (fun f -> Filename.check_suffix f ".nova")
    |> List.map (Filename.concat tests_dir)
    |> List.sort String.compare
  else []
;;

let collect_snapshot file =
  let parse_r = capture_output (fun () -> string_of_parse false [ file ]) in
  let codegen_r = capture_output (fun () -> string_of_codegen false [ file ]) in
  let run_r = run_with_status false [ file ] in
  let run, code =
    match run_r with
    | Run_ok (code, output) -> output, code
    | Run_error (_, code, msg) -> "ERROR: " ^ msg, code
  in
  { file
  ; parse = string_of_result parse_r
  ; codegen = string_of_result codegen_r
  ; run
  ; code
  }
;;

let truncate ?(limit = 120) s =
  if String.length s <= limit then s else String.sub s 0 limit ^ "…"
;;

let pp_field label value = Printf.sprintf "%s: %s" label (truncate value)

let pp_snapshot snap =
  [ pp_field "parse" snap.parse
  ; pp_field "codegen" snap.codegen
  ; pp_field "run" snap.run
  ; pp_field "code" (string_of_int snap.code)
  ]
  |> String.concat "\n"
;;

let read_expected_snapshots () =
  if Sys.file_exists expected_file
  then (
    match Json.from_file expected_file with
    | `List items ->
      items
      |> List.map test_snapshot_of_json
      |> List.sort (fun a b -> String.compare a.file b.file)
    | _ ->
      Utils.raise_error
        ~phase:Utils.Io
        ~file:expected_file
        ~row:0
        ~col:0
        "Expected a JSON list in .expected.json")
  else []
;;

let write_expected_snapshots snapshots =
  let json = `List (List.map test_snapshot_to_json snapshots) in
  let oc = open_out expected_file in
  Fun.protect
    (fun () ->
       Json.pretty_to_channel oc json;
       output_char oc '\n')
    ~finally:(fun () -> close_out oc)
;;

let snapshot_map snapshots =
  List.fold_left
    (fun acc snap -> String_map.add snap.file snap acc)
    String_map.empty
    snapshots
;;

let diff_snapshots expected actual =
  let expected_map = snapshot_map expected in
  let actual_map = snapshot_map actual in
  let keys m = String_map.fold (fun k _ acc -> k :: acc) m [] in
  let all_files = keys expected_map @ keys actual_map |> List.sort_uniq String.compare in
  let results =
    List.map
      (fun file ->
         match
           String_map.find_opt file expected_map, String_map.find_opt file actual_map
         with
         | None, Some _ -> `Unexpected file
         | Some _, None -> `Missing file
         | None, None -> `Missing file
         | Some exp, Some act ->
           let diffs =
             [ "parse", exp.parse, act.parse
             ; "codegen", exp.codegen, act.codegen
             ; "run", exp.run, act.run
             ; "code", string_of_int exp.code, string_of_int act.code
             ]
             |> List.filter (fun (_, a, b) -> not (String.equal a b))
           in
           if diffs = [] then `Pass file else `Diff (file, diffs))
      all_files
  in
  let failures =
    List.filter
      (function
        | `Pass _ -> false
        | _ -> true)
      results
  in
  results, failures
;;

let pp_diff (label, expected, actual) =
  Printf.sprintf
    "  - %s\n    expected: %s\n    actual:   %s"
    label
    (truncate expected)
    (truncate actual)
;;

let pp_result = function
  | `Pass file -> Printf.sprintf "[PASS] %s" file
  | `Missing file -> Printf.sprintf "[MISS] %s (expected but not generated)" file
  | `Unexpected file -> Printf.sprintf "[UNEXPECTED] %s" file
  | `Diff (file, diffs) ->
    let header = Printf.sprintf "[DIFF] %s" file in
    let body = diffs |> List.map pp_diff |> String.concat "\n" in
    header ^ "\n" ^ body
;;

let print_results results failures total =
  List.iter (fun r -> Printf.printf "%s\n" (pp_result r)) results;
  if failures = []
  then Printf.printf "\nAll %d tests passed\n" total
  else (
    Printf.printf "\n%d/%d tests failed\n" (List.length failures) total;
    exit 1)
;;

let process_test_compiler () =
  let files = nova_test_files () in
  if files = []
  then Printf.printf "No test files found in %s\n" tests_dir
  else (
    let actual = List.map collect_snapshot files in
    let expected = read_expected_snapshots () in
    if expected = []
    then (
      Printf.eprintf
        "No expectations found at %s. Run test-compiler-promote first.\n"
        expected_file;
      exit 1)
    else (
      let results, failures = diff_snapshots expected actual in
      print_results results failures (List.length files)))
;;

let process_test_compiler_promote () =
  let files = nova_test_files () in
  if files = []
  then Printf.printf "No test files found in %s\n" tests_dir
  else (
    let snapshots = List.map collect_snapshot files in
    write_expected_snapshots snapshots;
    Printf.printf "Updated expectations at %s\n" expected_file)
;;

let handle_novac_error f =
  try f () with
  | Utils.Novac_error (phase, err) ->
    let phase_str =
      match phase with
      | Utils.Lexer -> "Lexer"
      | Utils.Parser -> "Parser"
      | Utils.Analyzer -> "Analyzer"
      | Utils.Preprocessor -> "Preprocessor"
      | Utils.Optimizer -> "Optimizer"
      | Utils.Codegen -> "Codegen"
      | Utils.Io -> "IO"
    in
    Printf.eprintf "%s error at %s:%d:%d: %s\n" phase_str err.file err.row err.col err.msg;
    exit 1
;;

let parse_cmd =
  let doc = "Parse the input files." in
  let info = Cmd.info "parse" ~doc in
  Cmd.v
    info
    Term.(
      const (fun stdlib files ->
        handle_novac_error (fun () -> process_parse stdlib files))
      $ stdlib_flag
      $ files_arg)
;;

let codegen_cmd =
  let doc = "Generate code for the input files." in
  let info = Cmd.info "codegen" ~doc in
  Cmd.v
    info
    Term.(
      const (fun stdlib files ->
        handle_novac_error (fun () -> process_codegen stdlib files))
      $ stdlib_flag
      $ files_arg)
;;

let compile_cmd =
  let doc = "Compile the input files to an executable." in
  let info = Cmd.info "compile" ~doc in
  Cmd.v
    info
    Term.(
      const (fun stdlib files ->
        handle_novac_error (fun () -> process_compile stdlib files))
      $ stdlib_flag
      $ files_arg)
;;

let run_cmd =
  let doc = "Compile and run the input files." in
  let info = Cmd.info "run" ~doc in
  Cmd.v
    info
    Term.(
      const (fun stdlib files -> handle_novac_error (fun () -> process_run stdlib files))
      $ stdlib_flag
      $ files_arg)
;;

let clean_cmd =
  let doc = "Clean build artifacts." in
  let info = Cmd.info "clean" ~doc in
  Cmd.v info Term.(const process_clean $ const ())
;;

let test_compiler_cmd =
  let doc =
    "Run parse/codegen/run on ./tests/*.nova and compare against ./tests/.expected.json"
  in
  let info = Cmd.info "test-compiler" ~doc in
  Cmd.v info Term.(const (fun () -> handle_novac_error process_test_compiler) $ const ())
;;

let tc_cmd =
  let doc = "Alias for test-compiler" in
  let info = Cmd.info "tc" ~doc in
  Cmd.v info Term.(const (fun () -> handle_novac_error process_test_compiler) $ const ())
;;

let test_compiler_promote_cmd =
  let doc =
    "Capture parse/codegen/run outputs for ./tests/*.nova into ./tests/.expected.json"
  in
  let info = Cmd.info "test-compiler-promote" ~doc in
  Cmd.v
    info
    Term.(const (fun () -> handle_novac_error process_test_compiler_promote) $ const ())
;;

let tcp_cmd =
  let doc = "Alias for test-compiler-promote" in
  let info = Cmd.info "tcp" ~doc in
  Cmd.v
    info
    Term.(const (fun () -> handle_novac_error process_test_compiler_promote) $ const ())
;;

let main_cmd =
  let doc = "Supernova compiler." in
  let info = Cmd.info "novac" ~doc in
  Cmd.group
    info
    [ parse_cmd
    ; codegen_cmd
    ; compile_cmd
    ; run_cmd
    ; clean_cmd
    ; test_compiler_cmd
    ; tc_cmd
    ; test_compiler_promote_cmd
    ; tcp_cmd
    ]
;;
