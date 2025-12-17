let rec print_diff ppx xs =
  match xs with
  | x :: xt ->
    Format.printf "%s\n" (ppx x);
    print_diff ppx xt
  | [] -> ()
;;

let test node = print_diff Node.show_node node

let%expect_test "test spec" =
  let tokens = Lexer.lex Spec.content in
  let nodes = Parser.parse tokens in
  test nodes;
  [%expect {|
    (Node.Open_mod "Testing")
    (Node.Open_mod "Panic")
    Node.Decl {name = "add";
      params =
      [("a", (Some (Node.Type_ident "i32")));
        ("b", (Some (Node.Type_ident "i32")))];
      ret_type = (Some (Node.Type_ident "i32"));
      body = [(Node.Expr [(Token.Ident "a"); Token.Plus; (Token.Ident "b")])]}
    Node.Decl {name = "sub"; params = [("a", None); ("b", None)];
      ret_type = (Some (Node.Type_ident "i32"));
      body =
      [(Node.Expr
          [Token.Return; (Token.Ident "a"); Token.Dash; (Token.Ident "b")])
        ]}
    Node.Decl {name = "mul"; params = [("a", None); ("b", None)];
      ret_type = (Some (Node.Type_ident "i32"));
      body = [(Node.Expr [(Token.Ident "a"); Token.Star; (Token.Ident "b")])]}
    Node.Curry_Decl {name = "mul_5"; target_fn = "mul";
      args = [(Node.Positional [(Node.Expr [(Token.Number 5)])])]}
    (Node.Tag "recursive")
    Node.Decl {name = "factorial"; params = [("of", None)];
      ret_type = (Some (Node.Type_ident "i32"));
      body =
      [(Node.Expr
          [Token.Match; (Token.Ident "of"); Token.With; Token.Bar;
            (Token.Number 0); Token.Skinny_arrow; (Token.Number 1); Token.Bar;
            (Token.Ident "x"); Token.If; (Token.Ident "x"); Token.Lesser;
            (Token.Number 0); Token.Skinny_arrow; (Token.Ident "panic");
            Token.Bang; Token.Open_paren;
            (Token.String
               "factroial is not defined for negative numbers: got: {}");
            Token.Comma; (Token.Ident "x"); Token.Close_paren; Token.Bar;
            (Token.Ident "x"); Token.Skinny_arrow; (Token.Ident "x"); Token.Star;
            (Token.Ident "factorial"); Token.Open_paren; (Token.Ident "x");
            Token.Dash; (Token.Number 1); Token.Close_paren])
        ]}
    Node.Decl {name = "main"; params = []; ret_type = None;
      body =
      [(Node.Expr
          [Token.Let; (Token.Ident "res"); Token.Eql; (Token.Ident "add");
            Token.Open_paren; (Token.Number 1); Token.Comma; (Token.Number 5);
            Token.Close_paren]);
        (Node.Expr
           [(Token.Ident "assert"); Token.Bang; Token.Open_paren;
             (Token.Ident "res"); Token.Double_eql; (Token.Number 6);
             Token.Close_paren]);
        (Node.Expr
           [Token.Let; (Token.Ident "res"); Token.Eql; (Token.Ident "sub");
             Token.Open_paren; (Token.Number 5); Token.Comma; (Token.Number 15);
             Token.Close_paren]);
        (Node.Expr
           [(Token.Ident "assert"); Token.Bang; Token.Open_paren;
             (Token.Ident "res"); Token.Double_eql; Token.Dash;
             (Token.Number 10); Token.Close_paren]);
        (Node.Expr
           [Token.Let; (Token.Ident "res"); Token.Eql; (Token.Ident "mul");
             Token.Open_paren; (Token.Number 2); Token.Comma; (Token.Number 5);
             Token.Close_paren]);
        (Node.Expr
           [(Token.Ident "assert"); Token.Bang; Token.Open_paren;
             (Token.Ident "res"); Token.Double_eql; (Token.Number 10);
             Token.Close_paren]);
        (Node.Expr
           [Token.Let; (Token.Ident "res_curry"); Token.Eql;
             (Token.Ident "mul_5"); Token.Open_paren; (Token.Number 10);
             Token.Close_paren]);
        (Node.Expr
           [(Token.Ident "assert"); Token.Bang; Token.Open_paren;
             (Token.Ident "res_curry"); Token.Double_eql; (Token.Number 10);
             Token.Star; (Token.Number 5); Token.Close_paren])
        ]}
    |}]
;;
