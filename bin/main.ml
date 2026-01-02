open Novac
open Cmdliner

let () =
  let exit_code =
    try Cmd.eval Cli.main_cmd with
    | Novac.Utils.Novac_error (phase, err) ->
      let phase_str =
        match phase with
        | Novac.Utils.Lexer -> "Lexer"
        | Novac.Utils.Parser -> "Parser"
        | Novac.Utils.Analyzer -> "Analyzer"
        | Novac.Utils.Preprocessor -> "Preprocessor"
        | Novac.Utils.Optimizer -> "Optimizer"
        | Novac.Utils.Codegen -> "Codegen"
        | Novac.Utils.Io -> "IO"
      in
      Printf.eprintf
        "%s error at %s:%d:%d: %s\n"
        phase_str
        err.file
        err.row
        err.col
        err.msg;
      1
    | exn ->
      Printf.eprintf "Unexpected error: %s\n" (Printexc.to_string exn);
      1
  in
  exit exit_code
;;
