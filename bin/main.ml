open Novac
open Cmdliner

let () =
  let exit_code =
    try Cmd.eval Cli.main_cmd with
    | exn ->
      Printf.eprintf "Unexpected error: %s\n" (Printexc.to_string exn);
      1
  in
  exit exit_code
;;
