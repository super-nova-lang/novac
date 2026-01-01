let%expect_test "lexer_examples" =
  List.iter
    (fun (name, content) ->
       let _ = Lexer.lex (name ^ ".nova") content in
       Format.printf "lex %s ok\n" name)
    Nova_tests.all;
  [%expect
    {|
    lex showcase ok
    lex showcase ok
    |}]
;;
