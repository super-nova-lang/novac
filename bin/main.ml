[@@@warning "-32"]

open Novac
(* open Node *)

let print_nodes nodes =
  List.iter
    (fun n ->
       match n with
       | Node.Error s -> Logger.par#error "Error: %s" s
       | n -> Logger.par#info "found: %s" (Node.show n))
    nodes
;;

let () =
  match Array.length Sys.argv, Sys.argv with
  | 2, args ->
    let tokens = Lexer.lex_from_file args.(1) in
    let nodes = Parser.parse tokens in
    print_nodes nodes
  | x, _ -> Logger.log#error "Expected 1 args, got %d" (x - 1)
;;
