open Novac

let () =
  match Array.length Sys.argv, Sys.argv with
  | 2, args ->
    let tokens = Lexer.lex_from_file args.(1) in
    let _ = Parser.parse (Parser.create tokens) in
    (* let codegen = Codegen.codegen nodes in *)
    (* let _ = codegen in *)
    ()
  | x, _ -> Logger.log#error "Expected 1 args, got %d" (x - 1)
;;
