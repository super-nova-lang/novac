let rec print_diff ppx xs =
  match xs with
  | x :: xt ->
    Format.printf "%s\n" (ppx x);
    print_diff ppx xt
  | [] -> ()
;;

let test toks = print_diff Token.show toks

let%expect_test "test spec" =
  let tokens = Lexer.lex Spec.content in
  test tokens;
  [%expect
    {|
    Token.Open
    (Token.Ident "Testing")
    Token.Open
    (Token.Ident "Panic")
    Token.Let
    (Token.Ident "add")
    Token.Double_colon
    (Token.Ident "a")
    Token.Colon
    (Token.Ident "i32")
    Token.Comma
    (Token.Ident "b")
    Token.Colon
    (Token.Ident "i32")
    Token.Skinny_arrow
    (Token.Ident "i32")
    Token.Eql
    Token.Open_brack
    (Token.Ident "a")
    Token.Plus
    (Token.Ident "b")
    Token.Close_brack
    Token.Let
    (Token.Ident "sub")
    Token.Double_colon
    (Token.Ident "a")
    Token.Comma
    (Token.Ident "b")
    Token.Eql
    Token.Return
    (Token.Ident "a")
    Token.Dash
    (Token.Ident "b")
    Token.Semi_colon
    Token.Let
    (Token.Ident "mul")
    Token.Double_colon
    (Token.Ident "a")
    Token.Comma
    (Token.Ident "b")
    Token.Eql
    Token.Open_brack
    (Token.Ident "a")
    Token.Star
    (Token.Ident "b")
    Token.Close_brack
    Token.Let
    (Token.Ident "mul_5")
    Token.Double_colon
    (Token.Ident "mul")
    Token.Back_arrow
    (Token.Number 5)
    Token.Semi_colon
    Token.Hash
    Token.Open_square
    (Token.Ident "recursive")
    Token.Close_square
    Token.Let
    (Token.Ident "factorial")
    Token.Double_colon
    (Token.Ident "of")
    Token.Eql
    Token.Open_brack
    Token.Match
    (Token.Ident "of")
    Token.With
    Token.Bar
    (Token.Number 0)
    Token.Skinny_arrow
    (Token.Number 1)
    Token.Bar
    (Token.Ident "x")
    Token.If
    (Token.Ident "x")
    Token.Lesser
    (Token.Number 0)
    Token.Skinny_arrow
    (Token.Ident "panic")
    Token.Bang
    Token.Open_paren
    (Token.String "factroial is not defined for negative numbers: got: {}")
    Token.Comma
    (Token.Ident "x")
    Token.Close_paren
    Token.Bar
    (Token.Ident "x")
    Token.Skinny_arrow
    (Token.Ident "x")
    Token.Star
    (Token.Ident "factorial")
    Token.Open_paren
    (Token.Ident "x")
    Token.Dash
    (Token.Number 1)
    Token.Close_paren
    Token.Close_brack
    Token.Let
    (Token.Ident "main")
    Token.Double_colon
    Token.Open_paren
    Token.Close_paren
    Token.Eql
    Token.Open_brack
    Token.Let
    (Token.Ident "res")
    Token.Eql
    (Token.Ident "add")
    Token.Open_paren
    (Token.Number 1)
    Token.Comma
    (Token.Number 5)
    Token.Close_paren
    Token.Semi_colon
    (Token.Ident "assert")
    Token.Bang
    Token.Open_paren
    (Token.Ident "res")
    Token.Double_eql
    (Token.Number 6)
    Token.Close_paren
    Token.Semi_colon
    Token.Let
    (Token.Ident "res")
    Token.Eql
    (Token.Ident "sub")
    Token.Open_paren
    (Token.Number 5)
    Token.Comma
    (Token.Number 15)
    Token.Close_paren
    Token.Semi_colon
    (Token.Ident "assert")
    Token.Bang
    Token.Open_paren
    (Token.Ident "res")
    Token.Double_eql
    Token.Dash
    (Token.Number 10)
    Token.Close_paren
    Token.Semi_colon
    Token.Let
    (Token.Ident "res")
    Token.Eql
    (Token.Ident "mul")
    Token.Open_paren
    (Token.Number 2)
    Token.Comma
    (Token.Number 5)
    Token.Close_paren
    Token.Semi_colon
    (Token.Ident "assert")
    Token.Bang
    Token.Open_paren
    (Token.Ident "res")
    Token.Double_eql
    (Token.Number 10)
    Token.Close_paren
    Token.Semi_colon
    Token.Let
    (Token.Ident "res_curry")
    Token.Eql
    (Token.Ident "mul_5")
    Token.Open_paren
    (Token.Number 10)
    Token.Close_paren
    Token.Semi_colon
    (Token.Ident "assert")
    Token.Bang
    Token.Open_paren
    (Token.Ident "res_curry")
    Token.Double_eql
    (Token.Number 10)
    Token.Star
    (Token.Number 5)
    Token.Close_paren
    Token.Semi_colon
    Token.Close_brack
    Token.Eof
    |}]
;;
