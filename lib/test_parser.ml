let%expect_test "parser_examples" =
  List.iter
    (fun (name, content) ->
       let tokens = Lexer.lex (name ^ ".nova") content in
       let nodes = Parser.parse (Parser.create tokens) in
       if
         List.exists
           (function
             | Ast.Error _ -> true
             | _ -> false)
           nodes
       then failwith ("parse failed for " ^ name)
       else Format.printf "parse %s ok\n" name)
    Nova_tests.all;
  [%expect
    {|
    parse if_basic ok
    parse showcase ok
    |}]
;;
