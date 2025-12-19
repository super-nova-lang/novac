open Token

let rec print_diff ~ppx xs =
  match xs with
  | (x, loc) :: xt ->
    Format.printf "%s:%d:%d: %s\n" loc.file loc.row loc.col (ppx x);
    print_diff ~ppx xt
  | [] -> ()
;;

let%expect_test "test spec" =
  let tokens = Lexer.lex "spec" Spec_test.content in
  print_diff ~ppx:Token.show tokens;
  [%expect
    {|
    spec:2:1: Token.Open
    spec:2:6: (Token.Ident "MyModule")
    spec:3:1: Token.Open
    spec:3:6: (Token.Ident "MyModule")
    spec:3:14: Token.Dot
    spec:3:15: (Token.Ident "InnerModule")
    spec:4:1: Token.Open
    spec:4:6: (Token.Ident "Panic")
    spec:4:12: Token.With
    spec:4:17: Token.Open_brack
    spec:4:19: (Token.Ident "panic")
    spec:4:25: Token.Close_brack
    spec:5:1: Token.Open
    spec:5:6: (Token.Ident "Assert")
    spec:5:13: Token.With
    spec:5:18: Token.Open_brack
    spec:6:3: (Token.Ident "assert")
    spec:6:9: Token.Comma
    spec:7:3: (Token.Ident "Static")
    spec:7:9: Token.Dot
    spec:7:10: (Token.Ident "assert_static")
    spec:7:23: Token.Comma
    spec:8:3: (Token.Ident "assert_eq")
    spec:8:13: Token.As
    spec:8:16: (Token.Ident "is_eq")
    spec:8:21: Token.Comma
    spec:9:1: Token.Close_brack
    spec:12:1: Token.Let
    spec:12:5: (Token.Ident "add")
    spec:12:9: Token.Double_colon
    spec:12:12: (Token.Ident "a")
    spec:12:13: Token.Colon
    spec:12:15: (Token.Ident "i32")
    spec:12:18: Token.Comma
    spec:12:20: (Token.Ident "b")
    spec:12:21: Token.Colon
    spec:12:23: (Token.Ident "i32")
    spec:12:27: Token.Eql
    spec:12:29: (Token.Ident "a")
    spec:12:31: Token.Plus
    spec:12:33: (Token.Ident "b")
    spec:13:1: Token.Let
    spec:13:5: (Token.Ident "sub")
    spec:13:9: Token.Double_colon
    spec:13:12: (Token.Ident "a")
    spec:13:13: Token.Comma
    spec:13:15: (Token.Ident "b")
    spec:13:17: Token.Skinny_arrow
    spec:13:20: (Token.Ident "i32")
    spec:13:24: Token.Eql
    spec:13:26: (Token.Ident "a")
    spec:13:28: Token.Dash
    spec:13:30: (Token.Ident "b")
    spec:15:1: Token.Let
    spec:15:5: (Token.Ident "mul")
    spec:15:16: Token.Double_colon
    spec:15:19: (Token.Ident "a")
    spec:15:20: Token.Comma
    spec:15:22: (Token.Ident "b")
    spec:15:24: Token.Skinny_arrow
    spec:15:27: (Token.Ident "i32")
    spec:15:31: Token.Eql
    spec:15:33: (Token.Ident "a")
    spec:15:35: Token.Star
    spec:15:37: (Token.Ident "b")
    spec:16:1: Token.Let
    spec:16:5: (Token.Ident "mul_by_5")
    spec:16:16: Token.Double_colon
    spec:16:19: (Token.Ident "mul")
    spec:16:23: Token.Back_arrow
    spec:16:26: (Token.Number 5)
    spec:17:1: Token.Let
    spec:17:5: (Token.Ident "is_just_15")
    spec:17:16: Token.Double_colon
    spec:17:19: (Token.Ident "mul")
    spec:17:23: Token.Back_arrow
    spec:17:26: (Token.Number 5)
    spec:17:27: Token.Comma
    spec:17:29: (Token.Number 3)
    spec:20:1: (Token.Ident "assert_static")
    spec:20:14: Token.Bang
    spec:20:15: Token.Open_paren
    spec:21:3: (Token.Ident "mul")
    spec:21:6: Token.Open_paren
    spec:21:7: (Token.Number 5)
    spec:21:8: Token.Comma
    spec:21:10: (Token.Number 3)
    spec:21:11: Token.Close_paren
    spec:21:13: Token.Double_eql
    spec:21:16: (Token.Ident "mul_by_5")
    spec:21:24: Token.Open_paren
    spec:21:25: (Token.Number 3)
    spec:21:26: Token.Close_paren
    spec:21:28: Token.And
    spec:22:3: (Token.Number 15)
    spec:22:6: Token.Double_eql
    spec:22:9: (Token.Ident "is_just_15")
    spec:22:19: Token.Open_paren
    spec:22:20: Token.Close_paren
    spec:23:1: Token.Close_paren
    spec:26:1: Token.Hash
    spec:26:2: Token.Open_square
    spec:26:3: (Token.Ident "recursive")
    spec:26:12: Token.Close_square
    spec:27:1: Token.Let
    spec:27:5: (Token.Ident "factorial")
    spec:27:15: Token.Double_colon
    spec:27:18: (Token.Ident "of")
    spec:27:21: Token.Skinny_arrow
    spec:27:24: (Token.Ident "i32")
    spec:27:28: Token.Eql
    spec:28:3: Token.Match
    spec:28:9: (Token.Ident "of")
    spec:28:12: Token.With
    spec:29:3: Token.Bar
    spec:29:5: (Token.Number 0)
    spec:29:7: Token.Skinny_arrow
    spec:29:10: (Token.Number 1)
    spec:30:3: Token.Bar
    spec:30:5: (Token.Ident "x")
    spec:30:7: Token.If
    spec:30:10: (Token.Ident "x")
    spec:30:12: Token.Lesser
    spec:30:14: (Token.Number 0)
    spec:30:16: Token.Skinny_arrow
    spec:31:5: (Token.Ident "panic")
    spec:31:10: Token.Bang
    spec:31:11: Token.Open_paren
    spec:32:7: (Token.String "factorial is not defined for negative numbers: got: {}")
    spec:32:63: Token.Comma
    spec:33:7: (Token.Ident "x")
    spec:34:5: Token.Close_paren
    spec:35:3: Token.Bar
    spec:35:5: (Token.Ident "x")
    spec:35:7: Token.Skinny_arrow
    spec:35:10: (Token.Ident "x")
    spec:35:12: Token.Star
    spec:35:14: (Token.Ident "factorial")
    spec:35:23: Token.Open_paren
    spec:35:24: (Token.Ident "x")
    spec:35:26: Token.Dash
    spec:35:28: (Token.Number 1)
    spec:35:29: Token.Close_paren
    spec:38:1: Token.Let
    spec:38:5: (Token.Ident "Job")
    spec:38:9: Token.Double_colon
    spec:39:3: (Token.Ident "salary")
    spec:39:9: Token.Comma
    spec:40:3: Token.Question
    spec:40:4: (Token.Ident "language")
    spec:40:13: Token.Eql
    spec:40:15: (Token.String "c++")
    spec:40:20: Token.Comma
    spec:41:1: Token.Eql
    spec:41:3: (Token.Ident "enum")
    spec:41:8: Token.Open_brack
    spec:42:3: (Token.Ident "programmer")
    spec:42:14: Token.Skinny_arrow
    spec:42:17: Token.Struct
    spec:42:24: Token.Open_brack
    spec:43:5: (Token.Ident "language")
    spec:43:14: Token.Colon
    spec:43:16: (Token.Ident "string")
    spec:43:22: Token.Comma
    spec:44:3: Token.Close_brack
    spec:44:4: Token.Comma
    spec:45:3: (Token.Ident "other")
    spec:45:9: Token.Skinny_arrow
    spec:45:12: (Token.Ident "string")
    spec:45:18: Token.Comma
    spec:46:3: (Token.Ident "sales_rep")
    spec:46:12: Token.Comma
    spec:47:1: Token.Close_brack
    spec:47:3: Token.With
    spec:47:8: Token.Open_brack
    spec:48:3: (Token.Ident "salary")
    spec:48:10: Token.Colon
    spec:48:12: (Token.Ident "i32")
    spec:49:1: Token.Close_brack
    spec:51:1: Token.Let
    spec:51:5: (Token.Ident "Person")
    spec:51:12: Token.Double_colon
    spec:52:3: (Token.Ident "name")
    spec:52:7: Token.Comma
    spec:52:9: (Token.Ident "age")
    spec:52:12: Token.Comma
    spec:52:14: (Token.Ident "job")
    spec:53:1: Token.Eql
    spec:53:3: Token.Struct
    spec:53:10: Token.Open_brack
    spec:54:3: (Token.Ident "name")
    spec:54:8: Token.Colon
    spec:54:10: (Token.Ident "string")
    spec:54:17: Token.Eql
    spec:54:19: (Token.Ident "name")
    spec:54:23: Token.Comma
    spec:55:3: (Token.Ident "age")
    spec:55:8: Token.Colon
    spec:55:10: (Token.Ident "u8")
    spec:55:17: Token.Eql
    spec:55:19: (Token.Ident "age")
    spec:55:22: Token.Comma
    spec:56:3: (Token.Ident "job")
    spec:56:8: Token.Colon
    spec:56:10: (Token.Ident "job")
    spec:56:17: Token.Eql
    spec:56:19: (Token.Ident "job")
    spec:56:22: Token.Comma
    spec:57:1: Token.Close_brack
    spec:59:1: Token.Let
    spec:59:5: (Token.Ident "based_dev")
    spec:59:15: Token.Eql
    spec:59:17: (Token.Ident "Person")
    spec:59:23: Token.Open_paren
    spec:60:3: (Token.String "Ashton")
    spec:60:11: Token.Comma
    spec:60:13: (Token.Number 19)
    spec:60:15: Token.Comma
    spec:61:3: (Token.Ident "Job")
    spec:61:6: Token.Dot
    spec:61:7: (Token.Ident "programmer")
    spec:61:17: Token.Open_paren
    spec:61:18: (Token.Number 1)
    spec:61:19: Token.Low_dash
    spec:61:20: (Token.Number 0)
    spec:61:23: Token.Comma
    spec:61:25: (Token.String "nova")
    spec:61:31: Token.Close_paren
    spec:62:1: Token.Close_paren
    spec:64:1: Token.Let
    spec:64:5: (Token.Ident "prob_some_creep")
    spec:64:21: Token.Eql
    spec:64:23: (Token.Ident "Person")
    spec:64:29: Token.Open_paren
    spec:65:3: (Token.String "Joe Shmoe")
    spec:65:14: Token.Comma
    spec:65:16: (Token.Number 37)
    spec:65:18: Token.Comma
    spec:66:3: Token.Dot
    spec:66:4: (Token.Ident "sales_rep")
    spec:66:13: Token.Open_paren
    spec:66:14: Token.Close_paren
    spec:66:15: Token.Comma
    spec:67:1: Token.Close_paren
    spec:89:1: Token.Eof
    |}]
;;

let%expect_test "test person" =
  let tokens = Lexer.lex "person" Person_test.content in
  print_diff ~ppx:Token.show tokens;
  [%expect
    {|
    person:1:1: Token.Hash
    person:1:2: Token.Open_square
    person:1:3: (Token.Ident "mutable")
    person:1:10: Token.Open_paren
    person:1:11: (Token.Ident "age")
    person:1:14: Token.Comma
    person:1:16: (Token.Ident "income")
    person:1:22: Token.Close_paren
    person:1:23: Token.Close_square
    person:2:1: Token.Hash
    person:2:2: Token.Open_square
    person:2:3: (Token.Ident "derive")
    person:2:9: Token.Open_paren
    person:2:10: (Token.Ident "Showing")
    person:2:17: Token.Close_paren
    person:2:18: Token.Close_square
    person:3:1: Token.Let
    person:3:5: (Token.Ident "Person")
    person:3:12: Token.Double_colon
    person:4:3: (Token.Ident "name")
    person:4:7: Token.Comma
    person:4:9: (Token.Ident "age")
    person:4:12: Token.Comma
    person:4:14: Token.Question
    person:4:15: (Token.Ident "income")
    person:4:22: Token.Eql
    person:4:24: (Token.Number 1000)
    person:5:1: Token.Eql
    person:5:3: Token.Return
    person:5:10: Token.Struct
    person:5:17: Token.Open_brack
    person:6:3: (Token.Ident "name")
    person:6:9: Token.Colon
    person:6:11: (Token.Ident "string")
    person:6:18: Token.Eql
    person:6:20: (Token.Ident "name")
    person:6:24: Token.Comma
    person:7:3: (Token.Ident "age")
    person:7:9: Token.Colon
    person:7:11: (Token.Ident "i8")
    person:7:18: Token.Eql
    person:7:20: (Token.Ident "age")
    person:7:23: Token.Comma
    person:8:3: (Token.Ident "income")
    person:8:9: Token.Colon
    person:8:11: (Token.Ident "i32")
    person:8:18: Token.Eql
    person:8:20: (Token.Ident "income")
    person:8:26: Token.Comma
    person:9:1: Token.Close_brack
    person:12:1: Token.Let
    person:12:5: (Token.Ident "main")
    person:12:10: Token.Double_colon
    person:12:13: Token.Open_paren
    person:12:14: Token.Close_paren
    person:12:16: Token.Eql
    person:12:18: Token.Open_brack
    person:13:3: Token.Let
    person:13:7: (Token.Ident "p1")
    person:13:10: Token.Eql
    person:13:12: (Token.Ident "Person")
    person:13:18: Token.Open_paren
    person:13:19: (Token.String "Ashton")
    person:13:27: Token.Comma
    person:13:29: (Token.Number 19)
    person:13:31: Token.Comma
    person:13:33: (Token.Ident "income")
    person:13:40: Token.Double_colon
    person:13:43: (Token.Number 0)
    person:13:44: Token.Close_paren
    person:14:3: (Token.Ident "println")
    person:14:10: Token.Bang
    person:14:11: Token.Open_paren
    person:14:12: (Token.String "p1: {s}")
    person:14:21: Token.Comma
    person:14:23: (Token.Ident "p1")
    person:14:25: Token.Dot
    person:14:26: (Token.Ident "show")
    person:14:30: Token.Open_paren
    person:14:31: Token.Close_paren
    person:14:32: Token.Close_paren
    person:15:1: Token.Close_brack
    person:16:1: Token.Eof
    |}]
;;
