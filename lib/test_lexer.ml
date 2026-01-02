let%expect_test "lexer_examples" =
  List.iter
    (fun (name, content) ->
       let _ = Lexer.lex (name ^ ".nova") content in
       Format.printf "lex %s ok\n" name)
    Nova_tests.all;
  [%expect
    {|
    lex compound_assign ok
    lex c_style_for_loop ok
    lex enum_payload ok
    lex for_loop_test ok
    lex macros ok
    lex match_basic ok
    lex power ok
    lex showcase ok
    lex simple_for_loop ok
    lex while_basic ok
    |}]
;;
