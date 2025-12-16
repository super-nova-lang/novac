open Novac
open Logger

let () =
  match Array.length Sys.argv, Sys.argv with
  | 2, args ->
    let tokens = Lexer.lex_from_file args.(1) in
    List.iter
      (fun x ->
         match x with
         | Token.Unknown x -> log#error "Unknown: `%c`" x
         | x -> log#info "Found: %s" (Token.show x))
      tokens
  | x, _ -> log#error "Expected 1 args, got %d" (x - 1)
;;
