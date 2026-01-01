let%expect_test "lexer_examples" =
  List.iter
    (fun (name, content) ->
       let _ = Lexer.lex (name ^ ".nova") content in
       Format.printf "lex %s ok\n" name)
    Nova_tests.all;
  [%expect
    {|
    lex enum_payload ok
    lex macro_simple_test ok
    lex macros ok
    lex macros_simple ok
    lex macro_test ok
    lex match_basic ok
    lex power ok
    lex showcase ok
    lex while_basic ok
    |}]
;;
