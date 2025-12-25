open Novac
open Cmdliner

let () = exit (Cmd.eval Cli.main_cmd)
