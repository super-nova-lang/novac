open Novac

let () =
  match Array.length Sys.argv, Sys.argv with
  | 2, args ->
    let tokens = Lexer.lex_from_file args.(1) in
    List.iter
      (fun x ->
         match x with
         | Token.Unknown x -> Logger.lex#error "Unknown: `%c`" x
         | x -> Logger.lex#info "Found: %s" (Token.show x))
      tokens;
    Logger.lex#info "Found: %d tokens!" (List.length tokens)
  | x, _ -> Logger.log#error "Expected 1 args, got %d" (x - 1)
;;
