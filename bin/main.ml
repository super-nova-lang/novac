open Novac
open Cmdliner

let () =
	let exit_code =
		try Cmd.eval Cli.main_cmd with
		| Novac.Codegen.Error msg ->
			Printf.eprintf "Codegen error: %s\n" msg;
			1
		| Failure msg ->
			Printf.eprintf "Error: %s\n" msg;
			1
		| exn ->
			Printf.eprintf "Unexpected error: %s\n" (Printexc.to_string exn);
			1
	in
	exit exit_code
