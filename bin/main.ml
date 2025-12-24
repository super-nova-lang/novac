open Novac

let () =
  match Array.length Sys.argv, Sys.argv with
  | 2, args ->
    let tokens = Lexer.lex_from_file args.(1) in
    let nodes = Parser.parse (Parser.create tokens) in
    List.iter Codegen.codegen nodes;
    Llvm.dump_module Codegen.the_module;
    ()
  | x, _ -> Logger.log#error "Expected 1 args, got %d" (x - 1)
;;
