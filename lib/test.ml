let test_lexer (name, content) =
  Format.printf "File: %s\n" name;
  let tokens = Lexer.lex "test.nova" content in
  List.iter (fun (x, _) -> Format.printf "found: %s\n" (Token.show x)) tokens
;;

let test_parser (name, content) =
  Format.printf "File: %s\n" name;
  let tokens = Lexer.lex "test.nova" content in
  let nodes = Parser.parse (Parser.create tokens) in
  List.iter (fun x -> Format.printf "found: %s\n" (Node.show x)) nodes
;;

let%expect_test "lexer" =
  List.iter test_lexer Nova_tests.all;
  [%expect
    {|
    File: basic_functions
    found: Token.Let
    found: (Token.Ident "add")
    found: Token.Double_colon
    found: (Token.Ident "a")
    found: Token.Colon
    found: (Token.Ident "i32")
    found: Token.Comma
    found: (Token.Ident "b")
    found: Token.Colon
    found: (Token.Ident "i32")
    found: Token.Eql
    found: (Token.Ident "a")
    found: Token.Plus
    found: (Token.Ident "b")
    found: Token.Let
    found: (Token.Ident "sub")
    found: Token.Double_colon
    found: (Token.Ident "a")
    found: Token.Comma
    found: (Token.Ident "b")
    found: Token.Skinny_arrow
    found: (Token.Ident "i32")
    found: Token.Eql
    found: (Token.Ident "a")
    found: Token.Dash
    found: (Token.Ident "b")
    found: Token.Let
    found: (Token.Ident "mul")
    found: Token.Double_colon
    found: (Token.Ident "a")
    found: Token.Comma
    found: (Token.Ident "b")
    found: Token.Skinny_arrow
    found: (Token.Ident "i32")
    found: Token.Eql
    found: (Token.Ident "a")
    found: Token.Star
    found: (Token.Ident "b")
    found: Token.Let
    found: (Token.Ident "my_complex_fn")
    found: Token.Double_colon
    found: (Token.Ident "a")
    found: Token.Comma
    found: (Token.Ident "b")
    found: Token.Comma
    found: (Token.Ident "c")
    found: Token.Skinny_arrow
    found: (Token.Ident "i32")
    found: Token.Eql
    found: Token.Open_brack
    found: Token.Let
    found: (Token.Ident "res1")
    found: Token.Eql
    found: (Token.Ident "a")
    found: Token.Plus
    found: (Token.Ident "b")
    found: Token.Star
    found: (Token.Number 2)
    found: Token.Semi_colon
    found: Token.Let
    found: (Token.Ident "res2")
    found: Token.Eql
    found: (Token.Ident "b")
    found: Token.Forward_slash
    found: (Token.Ident "res1")
    found: Token.Semi_colon
    found: Token.Let
    found: (Token.Ident "res3")
    found: Token.Eql
    found: (Token.Ident "c")
    found: Token.Carrot
    found: Token.Open_paren
    found: (Token.Ident "res1")
    found: Token.Dash
    found: (Token.Number 55)
    found: Token.Close_paren
    found: Token.Semi_colon
    found: (Token.Ident "res3")
    found: Token.Close_brack
    found: Token.Eof
    File: complex_types
    found: Token.Hash
    found: Token.Open_square
    found: (Token.Ident "derive")
    found: Token.Open_paren
    found: (Token.Ident "Show")
    found: Token.Comma
    found: (Token.Ident "PrettyPrint")
    found: Token.Close_paren
    found: Token.Close_square
    found: Token.Let
    found: (Token.Ident "Job")
    found: Token.Double_colon
    found: (Token.Ident "salary")
    found: Token.Comma
    found: Token.Question
    found: (Token.Ident "language")
    found: Token.Eql
    found: (Token.String "c++")
    found: Token.Comma
    found: Token.Eql
    found: (Token.Ident "enum")
    found: Token.Open_brack
    found: (Token.Ident "programmer")
    found: Token.Skinny_arrow
    found: Token.Struct
    found: Token.Open_brack
    found: (Token.Ident "language")
    found: Token.Colon
    found: (Token.Ident "string")
    found: Token.Comma
    found: Token.Close_brack
    found: Token.Comma
    found: (Token.Ident "other")
    found: Token.Skinny_arrow
    found: (Token.Ident "string")
    found: Token.Comma
    found: (Token.Ident "sales_rep")
    found: Token.Comma
    found: Token.Close_brack
    found: Token.With
    found: Token.Open_brack
    found: (Token.Ident "salary")
    found: Token.Colon
    found: (Token.Ident "i32")
    found: (Token.Ident "hourly")
    found: Token.Double_colon
    found: (Token.Ident "self")
    found: Token.Comma
    found: (Token.Ident "hours")
    found: Token.Eql
    found: (Token.Ident "self")
    found: Token.Dot
    found: (Token.Ident "salary")
    found: Token.Forward_slash
    found: (Token.Ident "hours")
    found: Token.Close_brack
    found: Token.Hash
    found: Token.Open_square
    found: (Token.Ident "derive")
    found: Token.Open_paren
    found: (Token.Ident "Show")
    found: Token.Comma
    found: (Token.Ident "PrettyPrint")
    found: Token.Close_paren
    found: Token.Close_square
    found: Token.Let
    found: (Token.Ident "Person")
    found: Token.Double_colon
    found: (Token.Ident "name")
    found: Token.Comma
    found: (Token.Ident "age")
    found: Token.Comma
    found: (Token.Ident "job")
    found: Token.Eql
    found: Token.Struct
    found: Token.Open_brack
    found: (Token.Ident "name")
    found: Token.Colon
    found: (Token.Ident "string")
    found: Token.Eql
    found: (Token.Ident "name")
    found: Token.Comma
    found: (Token.Ident "age")
    found: Token.Colon
    found: (Token.Ident "u8")
    found: Token.Eql
    found: (Token.Ident "age")
    found: Token.Comma
    found: (Token.Ident "job")
    found: Token.Colon
    found: (Token.Ident "job")
    found: Token.Eql
    found: (Token.Ident "job")
    found: Token.Comma
    found: Token.Close_brack
    found: Token.With
    found: Token.Open_brack
    found: Token.Let
    found: (Token.Ident "introduce")
    found: Token.Double_colon
    found: (Token.Ident "self")
    found: Token.Eql
    found: (Token.Ident "println")
    found: Token.Bang
    found: Token.Open_paren
    found: (Token.String "Hello, my name is {} and I am a {}")
    found: Token.Comma
    found: (Token.Ident "self")
    found: Token.Dot
    found: (Token.Ident "name")
    found: Token.Comma
    found: (Token.Ident "self")
    found: Token.Dot
    found: (Token.Ident "job")
    found: Token.Dot
    found: (Token.Ident "show")
    found: Token.UnitToken
    found: Token.Close_paren
    found: Token.Close_brack
    found: Token.Let
    found: (Token.Ident "based_dev")
    found: Token.Walrus
    found: (Token.Ident "Person")
    found: Token.Open_paren
    found: (Token.String "Ashton")
    found: Token.Comma
    found: (Token.Number 19)
    found: Token.Comma
    found: (Token.Ident "Job")
    found: Token.Dot
    found: (Token.Ident "programmer")
    found: Token.Open_paren
    found: (Token.Number 1)
    found: Token.Low_dash
    found: (Token.Number 0)
    found: Token.Comma
    found: (Token.String "nova")
    found: Token.Close_paren
    found: Token.Close_paren
    found: Token.Let
    found: (Token.Ident "prob_some_creep")
    found: Token.Walrus
    found: (Token.Ident "Person")
    found: Token.Open_paren
    found: (Token.String "Joe Shmoe")
    found: Token.Comma
    found: (Token.Number 37)
    found: Token.Comma
    found: Token.Dot
    found: (Token.Ident "sales_rep")
    found: Token.UnitToken
    found: Token.Comma
    found: Token.Close_paren
    found: Token.Let
    found: (Token.Ident "my_printf")
    found: Token.Double_colon
    found: (Token.Ident "fmt")
    found: Token.Comma
    found: (Token.Ident "args")
    found: Token.Dot
    found: Token.Dot
    found: Token.Dot
    found: Token.Eql
    found: (Token.Ident "macro")
    found: Token.Open_brack
    found: Token.Close_brack
    found: (Token.Ident "my_printf")
    found: Token.Bang
    found: Token.Open_paren
    found: (Token.String "Hello world!")
    found: Token.Close_paren
    found: (Token.Ident "my_printf")
    found: Token.Bang
    found: Token.Open_paren
    found: (Token.String "This is {}")
    found: Token.Comma
    found: (Token.String "nova")
    found: Token.Close_paren
    found: Token.Let
    found: (Token.Ident "my_derive")
    found: Token.Double_colon
    found: (Token.Ident "tt")
    found: Token.Eql
    found: (Token.Ident "derive")
    found: Token.Open_brack
    found: Token.Close_brack
    found: Token.Hash
    found: Token.Open_square
    found: (Token.Ident "derive")
    found: Token.Open_paren
    found: (Token.Ident "my_derive")
    found: Token.Close_paren
    found: Token.Close_square
    found: Token.Let
    found: (Token.Ident "some_struct")
    found: Token.Double_colon
    found: Token.UnitToken
    found: Token.Eql
    found: Token.Struct
    found: Token.Open_brack
    found: Token.Close_brack
    found: Token.Let
    found: (Token.Ident "my_derive_with_params")
    found: Token.Double_colon
    found: (Token.Ident "tt")
    found: Token.Comma
    found: (Token.Ident "a")
    found: Token.Comma
    found: (Token.Ident "b")
    found: Token.Eql
    found: (Token.Ident "derive")
    found: Token.Open_brack
    found: Token.Close_brack
    found: Token.Hash
    found: Token.Open_square
    found: (Token.Ident "derive")
    found: Token.Open_paren
    found: (Token.Ident "my_derive_with_params")
    found: Token.Open_paren
    found: (Token.String "a")
    found: Token.Comma
    found: (Token.String "b")
    found: Token.Close_paren
    found: Token.Close_paren
    found: Token.Close_square
    found: Token.Let
    found: (Token.Ident "some_struct")
    found: Token.Double_colon
    found: Token.UnitToken
    found: Token.Eql
    found: Token.Struct
    found: Token.Open_brack
    found: Token.Close_brack
    found: Token.Eof
    File: currying_functions
    found: Token.Let
    found: (Token.Ident "mul_5")
    found: Token.Double_colon
    found: (Token.Ident "mul")
    found: Token.Back_arrow
    found: (Token.Number 5)
    found: Token.Let
    found: (Token.Ident "just_15")
    found: Token.Double_colon
    found: (Token.Ident "mul")
    found: Token.Back_arrow
    found: (Token.Number 5)
    found: Token.Comma
    found: (Token.Number 3)
    found: Token.Eof
    File: open_statements
    found: Token.Open
    found: (Token.Ident "MyModule")
    found: Token.Open
    found: (Token.Ident "MyModule")
    found: Token.Dot
    found: (Token.Ident "SomeMod")
    found: Token.Open
    found: (Token.Ident "Panic")
    found: Token.With
    found: Token.Open_brack
    found: (Token.Ident "panic")
    found: Token.Close_brack
    found: Token.Open
    found: (Token.Ident "Assert")
    found: Token.With
    found: Token.Open_brack
    found: (Token.Ident "assert")
    found: Token.Comma
    found: (Token.Ident "Static")
    found: Token.Dot
    found: (Token.Ident "assert_static")
    found: Token.Comma
    found: (Token.Ident "assert_eq")
    found: Token.As
    found: (Token.Ident "is_eq")
    found: Token.Comma
    found: Token.Close_brack
    found: Token.Eof
    File: tags_and_macros
    found: Token.Hash
    found: Token.Open_square
    found: (Token.Ident "recusive")
    found: Token.Close_square
    found: Token.Hash
    found: Token.Open_square
    found: (Token.Ident "derive")
    found: Token.Open_paren
    found: (Token.Ident "Show")
    found: Token.Comma
    found: (Token.Ident "PrettyPrint")
    found: Token.Close_paren
    found: Token.Close_square
    found: Token.Let
    found: (Token.Ident "my_type")
    found: Token.Double_colon
    found: Token.UnitToken
    found: Token.Eql
    found: (Token.String "ahhh")
    found: Token.Eof
    |}]
;;

let%expect_test "parser" =
  List.iter test_parser Nova_tests.all;
  [%expect.unreachable]
[@@expect.uncaught_exn
  {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)
  (Failure "Unexpected token Token.Open_brack at test.nova:7:3")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Novac__Parser.get_ok_or_fail in file "lib/parser.ml" (inlined), line 34, characters 17-29
  Called from Novac__Parser.parse_multiplicative_expr in file "lib/parser.ml", line 438, characters 35-73
  Called from Novac__Parser.parse_additive_expr in file "lib/parser.ml", line 419, characters 30-57
  Called from Novac__Parser.parse_relational_expr in file "lib/parser.ml", line 389, characters 13-34
  Called from Novac__Parser.parse_expression_stmt in file "lib/parser.ml", line 357, characters 8-26
  Called from Novac__Parser.parse_toplevel in file "lib/parser.ml", line 68, characters 11-28
  Called from Novac__Parser.parse in file "lib/parser.ml", line 40, characters 10-26
  Called from Novac__Test.test_parser in file "lib/test.ml", line 10, characters 14-49
  Called from Stdlib__List.iter in file "list.ml", line 114, characters 12-15
  Called from Novac__Test.(fun) in file "lib/test.ml", line 368, characters 2-38
  Called from Ppx_expect_runtime__Test_block.Configured.dump_backtrace in file "runtime/test_block.ml", line 142, characters 10-28

  Trailing output
  ---------------
  File: basic_functions
  found: (Node.Statement
     (Node.Decl_stmt
        Node.Decl {tags = []; name = "add";
          params =
          [(Node.Typed ("a", (Node.User "i32")));
            (Node.Typed ("b", (Node.User "i32")))];
          explicit_ret = None;
          body =
          ([],
           (Some (Node.Relational_expr
                    (Node.Relational_val
                       (Node.Add (
                          (Node.Additive_val
                             (Node.Multiplicative_val
                                (Node.Unary_val (Node.Ident "a")))),
                          (Node.Multiplicative_val
                             (Node.Unary_val (Node.Ident "b")))
                          ))))))}))
  found: (Node.Statement
     (Node.Decl_stmt
        Node.Decl {tags = []; name = "sub";
          params = [(Node.Untyped "a"); (Node.Untyped "b")];
          explicit_ret = (Some (Node.User "i32"));
          body =
          ([],
           (Some (Node.Relational_expr
                    (Node.Relational_val
                       (Node.Sub (
                          (Node.Additive_val
                             (Node.Multiplicative_val
                                (Node.Unary_val (Node.Ident "a")))),
                          (Node.Multiplicative_val
                             (Node.Unary_val (Node.Ident "b")))
                          ))))))}))
  found: (Node.Statement
     (Node.Decl_stmt
        Node.Decl {tags = []; name = "mul";
          params = [(Node.Untyped "a"); (Node.Untyped "b")];
          explicit_ret = (Some (Node.User "i32"));
          body =
          ([],
           (Some (Node.Relational_expr
                    (Node.Relational_val
                       (Node.Additive_val
                          (Node.Mul (
                             (Node.Multiplicative_val
                                (Node.Unary_val (Node.Ident "a"))),
                             (Node.Unary_val (Node.Ident "b")))))))))}))
  found: (Node.Statement
     (Node.Decl_stmt
        Node.Decl {tags = []; name = "my_complex_fn";
          params = [(Node.Untyped "a"); (Node.Untyped "b"); (Node.Untyped "c")];
          explicit_ret = (Some (Node.User "i32"));
          body =
          ([(Node.Decl_stmt
               Node.Decl {tags = []; name = "res1"; params = [];
                 explicit_ret = None;
                 body =
                 ([],
                  (Some (Node.Relational_expr
                           (Node.Relational_val
                              (Node.Add (
                                 (Node.Additive_val
                                    (Node.Multiplicative_val
                                       (Node.Unary_val (Node.Ident "a")))),
                                 (Node.Mul (
                                    (Node.Multiplicative_val
                                       (Node.Unary_val (Node.Ident "b"))),
                                    (Node.Unary_val (Node.Int 2))))
                                 ))))))});
             (Node.Decl_stmt
                Node.Decl {tags = []; name = "res2"; params = [];
                  explicit_ret = None;
                  body =
                  ([],
                   (Some (Node.Relational_expr
                            (Node.Relational_val
                               (Node.Additive_val
                                  (Node.Div (
                                     (Node.Multiplicative_val
                                        (Node.Unary_val (Node.Ident "b"))),
                                     (Node.Unary_val (Node.Ident "res1")))))))))});
             (Node.Decl_stmt
                Node.Decl {tags = []; name = "res3"; params = [];
                  explicit_ret = None;
                  body =
                  ([],
                   (Some (Node.Relational_expr
                            (Node.Relational_val
                               (Node.Additive_val
                                  (Node.Pow (
                                     (Node.Multiplicative_val
                                        (Node.Unary_val (Node.Ident "c"))),
                                     (Node.Unary_val
                                        (Node.Grouping
                                           (Node.Relational_expr
                                              (Node.Relational_val
                                                 (Node.Sub (
                                                    (Node.Additive_val
                                                       (Node.Multiplicative_val
                                                          (Node.Unary_val
                                                             (Node.Ident "res1")))),
                                                    (Node.Multiplicative_val
                                                       (Node.Unary_val
                                                          (Node.Int 55)))
                                                    ))))))
                                     )))))))})
             ],
           (Some (Node.Relational_expr
                    (Node.Relational_val
                       (Node.Additive_val
                          (Node.Multiplicative_val
                             (Node.Unary_val (Node.Ident "res3"))))))))}))
  File: complex_types
  |}]
;;
