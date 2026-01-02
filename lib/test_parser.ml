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
    parse compound_assign ok
    parse c_style_for_loop ok
    parse enum_payload ok
    parse for_loop_test ok
    parse macros ok
    parse match_basic ok
    parse power ok
    parse showcase ok
    parse simple_for_loop ok
    parse while_basic ok
    |}]
;;
