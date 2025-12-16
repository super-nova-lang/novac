open Novac
(* open Node *)

let () =
  match Array.length Sys.argv, Sys.argv with
  | 2, args ->
    let tokens = Lexer.lex_from_file args.(1) in
    let nodes = Parser.parse tokens in
    List.iter
      (fun n ->
         match n with
         | Node.Unhandled t -> Logger.par#error "unhandled token: %s" (Token.show t)
         | t -> Logger.par#info "found: %s" (Node.show_node t))
      nodes
  | x, _ -> Logger.log#error "Expected 1 args, got %d" (x - 1)
;;
