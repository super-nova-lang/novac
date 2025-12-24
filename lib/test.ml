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

[@@@ocamlformat "disable"]
let%expect_test "lexer" =
  List.iter test_lexer Nova_tests.all;
  [%expect {|
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
    found: Token.Derive
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
    found: Token.Enum
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
    found: Token.Derive
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
    found: Token.Open_paren
    found: Token.Close_paren
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
    found: (Token.Number 1000)
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
    found: Token.Open_paren
    found: Token.Close_paren
    found: Token.Comma
    found: Token.Close_paren
    found: Token.Let
    found: (Token.Ident "my_printf")
    found: Token.Double_colon
    found: (Token.Ident "fmt")
    found: Token.Comma
    found: (Token.Ident "args")
    found: Token.Ellipsis
    found: Token.Eql
    found: Token.Macro
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
    found: Token.Derive
    found: Token.Open_brack
    found: Token.Close_brack
    found: Token.Hash
    found: Token.Open_square
    found: Token.Derive
    found: Token.Open_paren
    found: (Token.Ident "my_derive")
    found: Token.Close_paren
    found: Token.Close_square
    found: Token.Let
    found: (Token.Ident "some_struct")
    found: Token.Double_colon
    found: Token.Open_paren
    found: Token.Close_paren
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
    found: Token.Derive
    found: Token.Open_brack
    found: Token.Close_brack
    found: Token.Hash
    found: Token.Open_square
    found: Token.Derive
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
    found: Token.Open_paren
    found: Token.Close_paren
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
    found: Token.Derive
    found: Token.Open_paren
    found: (Token.Ident "Show")
    found: Token.Comma
    found: (Token.Ident "PrettyPrint")
    found: Token.Close_paren
    found: Token.Close_square
    found: Token.Let
    found: (Token.Ident "my_type")
    found: Token.Double_colon
    found: Token.Open_paren
    found: Token.Close_paren
    found: Token.Eql
    found: (Token.String "ahhh")
    found: Token.Eof
    |}]
;;

[@@@ocamlformat "disable"]
let%expect_test "parser" =
  List.iter test_parser Nova_tests.all;
  [%expect {|
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
    found: (Node.Statement
       (Node.Decl_stmt
          Node.Decl {
            tags =
            [(Node.Tag_call
                (Node.Decl_call (
                   (Node.Relational_expr
                      (Node.Relational_val
                         (Node.Additive_val
                            (Node.Multiplicative_val
                               (Node.Unary_val (Node.Ident "derive")))))),
                   [(Node.Positional
                       (Node.Relational_expr
                          (Node.Relational_val
                             (Node.Additive_val
                                (Node.Multiplicative_val
                                   (Node.Unary_val (Node.Ident "Show")))))));
                     (Node.Positional
                        (Node.Relational_expr
                           (Node.Relational_val
                              (Node.Additive_val
                                 (Node.Multiplicative_val
                                    (Node.Unary_val (Node.Ident "PrettyPrint")))))))
                     ]
                   )))
              ];
            name = "Job";
            params =
            [(Node.Untyped "salary");
              (Node.OptionalUntyped ("language",
                 (Node.Relational_expr
                    (Node.Relational_val
                       (Node.Additive_val
                          (Node.Multiplicative_val
                             (Node.Unary_val (Node.String "c++"))))))
                 ))
              ];
            explicit_ret = None;
            body =
            ([],
             (Some (Node.Enum_expr (
                      [("programmer",
                        (Some (Node.Struct_body
                                 [("language", (Node.User "string"), None)])));
                        ("other", (Some (Node.Type_body (Node.User "string"))));
                        ("sales_rep", None)],
                      (Some [(Node.Statement
                                (Node.Decl_stmt
                                   Node.Decl {tags = []; name = "salary";
                                     params = [];
                                     explicit_ret = (Some (Node.User "i32"));
                                     body = ([], None)}));
                              (Node.Statement
                                 (Node.Decl_stmt
                                    Node.Decl {tags = []; name = "hourly";
                                      params =
                                      [(Node.Untyped "self");
                                        (Node.Untyped "hours")];
                                      explicit_ret = None;
                                      body =
                                      ([],
                                       (Some (Node.Relational_expr
                                                (Node.Relational_val
                                                   (Node.Additive_val
                                                      (Node.Div (
                                                         (Node.Multiplicative_val
                                                            (Node.Unary_member (
                                                               (Node.Unary_val
                                                                  (Node.Ident
                                                                     "self")),
                                                               "salary"))),
                                                         (Node.Unary_val
                                                            (Node.Ident "hours"))
                                                         )))))))}))
                              ])
                      ))))}))
    found: (Node.Statement
       (Node.Decl_stmt
          Node.Decl {
            tags =
            [(Node.Tag_call
                (Node.Decl_call (
                   (Node.Relational_expr
                      (Node.Relational_val
                         (Node.Additive_val
                            (Node.Multiplicative_val
                               (Node.Unary_val (Node.Ident "derive")))))),
                   [(Node.Positional
                       (Node.Relational_expr
                          (Node.Relational_val
                             (Node.Additive_val
                                (Node.Multiplicative_val
                                   (Node.Unary_val (Node.Ident "Show")))))));
                     (Node.Positional
                        (Node.Relational_expr
                           (Node.Relational_val
                              (Node.Additive_val
                                 (Node.Multiplicative_val
                                    (Node.Unary_val (Node.Ident "PrettyPrint")))))))
                     ]
                   )))
              ];
            name = "Person";
            params =
            [(Node.Untyped "name"); (Node.Untyped "age"); (Node.Untyped "job")];
            explicit_ret = None;
            body =
            ([],
             (Some (Node.Struct_expr (
                      [("name", (Node.User "string"),
                        (Some (Node.Relational_expr
                                 (Node.Relational_val
                                    (Node.Additive_val
                                       (Node.Multiplicative_val
                                          (Node.Unary_val (Node.Ident "name"))))))));
                        ("age", (Node.User "u8"),
                         (Some (Node.Relational_expr
                                  (Node.Relational_val
                                     (Node.Additive_val
                                        (Node.Multiplicative_val
                                           (Node.Unary_val (Node.Ident "age"))))))));
                        ("job", (Node.User "job"),
                         (Some (Node.Relational_expr
                                  (Node.Relational_val
                                     (Node.Additive_val
                                        (Node.Multiplicative_val
                                           (Node.Unary_val (Node.Ident "job"))))))))
                        ],
                      (Some [(Node.Statement
                                (Node.Decl_stmt
                                   Node.Decl {tags = []; name = "introduce";
                                     params = [(Node.Untyped "self")];
                                     explicit_ret = None;
                                     body =
                                     ([],
                                      (Some (Node.Relational_expr
                                               (Node.Relational_val
                                                  (Node.Additive_val
                                                     (Node.Multiplicative_val
                                                        (Node.Unary_call
                                                           (Node.Macro_call (
                                                              (Node.Relational_expr
                                                                 (Node.Relational_val
                                                                    (Node.Additive_val
                                                                       (Node.Multiplicative_val
                                                                        (Node.Unary_val
                                                                        (Node.Ident
                                                                        "println")))))),
                                                              [(Node.Positional
                                                                  (Node.Relational_expr
                                                                     (Node.Relational_val
                                                                        (
                                                                        Node.Additive_val
                                                                        (Node.Multiplicative_val
                                                                        (Node.Unary_val
                                                                        (Node.String
                                                                        "Hello, my name is {} and I am a {}")))))));
                                                                (Node.Positional
                                                                   (Node.Relational_expr
                                                                      (Node.Relational_val
                                                                        (Node.Additive_val
                                                                        (Node.Multiplicative_val
                                                                        (Node.Unary_member (
                                                                        (Node.Unary_val
                                                                        (Node.Ident
                                                                        "self")),
                                                                        "name")))))));
                                                                (Node.Positional
                                                                   (Node.Relational_expr
                                                                      (Node.Relational_val
                                                                        (Node.Additive_val
                                                                        (Node.Multiplicative_val
                                                                        (Node.Unary_call
                                                                        (Node.Decl_call (
                                                                        (Node.Relational_expr
                                                                        (Node.Relational_val
                                                                        (Node.Additive_val
                                                                        (Node.Multiplicative_val
                                                                        (Node.Unary_member (
                                                                        (Node.Unary_member (
                                                                        (Node.Unary_val
                                                                        (Node.Ident
                                                                        "self")),
                                                                        "job")),
                                                                        "show")))))),
                                                                        []))))))))
                                                                ]
                                                              )))))))))}))
                              ])
                      ))))}))
    found: (Node.Statement
       (Node.Decl_stmt
          Node.Decl {tags = []; name = "based_dev"; params = [];
            explicit_ret = None;
            body =
            ([],
             (Some (Node.Relational_expr
                      (Node.Relational_val
                         (Node.Additive_val
                            (Node.Multiplicative_val
                               (Node.Unary_call
                                  (Node.Decl_call (
                                     (Node.Relational_expr
                                        (Node.Relational_val
                                           (Node.Additive_val
                                              (Node.Multiplicative_val
                                                 (Node.Unary_val
                                                    (Node.Ident "Person")))))),
                                     [(Node.Positional
                                         (Node.Relational_expr
                                            (Node.Relational_val
                                               (Node.Additive_val
                                                  (Node.Multiplicative_val
                                                     (Node.Unary_val
                                                        (Node.String "Ashton")))))));
                                       (Node.Positional
                                          (Node.Relational_expr
                                             (Node.Relational_val
                                                (Node.Additive_val
                                                   (Node.Multiplicative_val
                                                      (Node.Unary_val
                                                         (Node.Int 19)))))));
                                       (Node.Positional
                                          (Node.Relational_expr
                                             (Node.Relational_val
                                                (Node.Additive_val
                                                   (Node.Multiplicative_val
                                                      (Node.Unary_call
                                                         (Node.Decl_call (
                                                            (Node.Relational_expr
                                                               (Node.Relational_val
                                                                  (Node.Additive_val
                                                                     (Node.Multiplicative_val
                                                                        (
                                                                        Node.Unary_member (
                                                                        (Node.Unary_val
                                                                        (Node.Ident
                                                                        "Job")),
                                                                        "programmer"
                                                                        )))))),
                                                            [(Node.Positional
                                                                (Node.Relational_expr
                                                                   (Node.Relational_val
                                                                      (Node.Additive_val
                                                                        (Node.Multiplicative_val
                                                                        (Node.Unary_val
                                                                        (Node.Int
                                                                        1000)))))));
                                                              (Node.Positional
                                                                 (Node.Relational_expr
                                                                    (Node.Relational_val
                                                                       (Node.Additive_val
                                                                        (Node.Multiplicative_val
                                                                        (Node.Unary_val
                                                                        (Node.String
                                                                        "nova")))))))
                                                              ]
                                                            ))))))))
                                       ]
                                     )))))))))}))
    found: (Node.Statement
       (Node.Decl_stmt
          Node.Decl {tags = []; name = "prob_some_creep"; params = [];
            explicit_ret = None;
            body =
            ([],
             (Some (Node.Relational_expr
                      (Node.Relational_val
                         (Node.Additive_val
                            (Node.Multiplicative_val
                               (Node.Unary_call
                                  (Node.Decl_call (
                                     (Node.Relational_expr
                                        (Node.Relational_val
                                           (Node.Additive_val
                                              (Node.Multiplicative_val
                                                 (Node.Unary_val
                                                    (Node.Ident "Person")))))),
                                     [(Node.Positional
                                         (Node.Relational_expr
                                            (Node.Relational_val
                                               (Node.Additive_val
                                                  (Node.Multiplicative_val
                                                     (Node.Unary_val
                                                        (Node.String "Joe Shmoe")))))));
                                       (Node.Positional
                                          (Node.Relational_expr
                                             (Node.Relational_val
                                                (Node.Additive_val
                                                   (Node.Multiplicative_val
                                                      (Node.Unary_val
                                                         (Node.Int 37)))))));
                                       (Node.Positional
                                          (Node.Relational_expr
                                             (Node.Relational_val
                                                (Node.Additive_val
                                                   (Node.Multiplicative_val
                                                      (Node.Unary_call
                                                         (Node.Decl_call (
                                                            (Node.Relational_expr
                                                               (Node.Relational_val
                                                                  (Node.Additive_val
                                                                     (Node.Multiplicative_val
                                                                        (
                                                                        Node.Unary_val
                                                                        (Node.Implicit_member
                                                                        "sales_rep")))))),
                                                            []))))))))
                                       ]
                                     )))))))))}))
    found: (Node.Statement
       (Node.Decl_stmt
          Node.Decl {tags = []; name = "my_printf";
            params = [(Node.Untyped "fmt"); (Node.Variadic "args")];
            explicit_ret = None; body = ([], (Some (Node.Macro_expr [])))}))
    found: (Node.Statement
       (Node.Expression_stmt
          (Node.Relational_expr
             (Node.Relational_val
                (Node.Additive_val
                   (Node.Multiplicative_val
                      (Node.Unary_call
                         (Node.Macro_call (
                            (Node.Relational_expr
                               (Node.Relational_val
                                  (Node.Additive_val
                                     (Node.Multiplicative_val
                                        (Node.Unary_val (Node.Ident "my_printf")))))),
                            [(Node.Positional
                                (Node.Relational_expr
                                   (Node.Relational_val
                                      (Node.Additive_val
                                         (Node.Multiplicative_val
                                            (Node.Unary_val
                                               (Node.String "Hello world!")))))))
                              ]
                            )))))))))
    found: (Node.Statement
       (Node.Expression_stmt
          (Node.Relational_expr
             (Node.Relational_val
                (Node.Additive_val
                   (Node.Multiplicative_val
                      (Node.Unary_call
                         (Node.Macro_call (
                            (Node.Relational_expr
                               (Node.Relational_val
                                  (Node.Additive_val
                                     (Node.Multiplicative_val
                                        (Node.Unary_val (Node.Ident "my_printf")))))),
                            [(Node.Positional
                                (Node.Relational_expr
                                   (Node.Relational_val
                                      (Node.Additive_val
                                         (Node.Multiplicative_val
                                            (Node.Unary_val
                                               (Node.String "This is {}")))))));
                              (Node.Positional
                                 (Node.Relational_expr
                                    (Node.Relational_val
                                       (Node.Additive_val
                                          (Node.Multiplicative_val
                                             (Node.Unary_val (Node.String "nova")))))))
                              ]
                            )))))))))
    found: (Node.Statement
       (Node.Decl_stmt
          Node.Decl {tags = []; name = "my_derive";
            params = [(Node.Untyped "tt")]; explicit_ret = None;
            body = ([], (Some (Node.Derive_expr [])))}))
    found: (Node.Statement
       (Node.Decl_stmt
          Node.Decl {
            tags =
            [(Node.Tag_call
                (Node.Decl_call (
                   (Node.Relational_expr
                      (Node.Relational_val
                         (Node.Additive_val
                            (Node.Multiplicative_val
                               (Node.Unary_val (Node.Ident "derive")))))),
                   [(Node.Positional
                       (Node.Relational_expr
                          (Node.Relational_val
                             (Node.Additive_val
                                (Node.Multiplicative_val
                                   (Node.Unary_val (Node.Ident "my_derive")))))))
                     ]
                   )))
              ];
            name = "some_struct"; params = []; explicit_ret = None;
            body = ([], (Some (Node.Struct_expr ([], None))))}))
    found: (Node.Statement
       (Node.Decl_stmt
          Node.Decl {tags = []; name = "my_derive_with_params";
            params =
            [(Node.Untyped "tt"); (Node.Untyped "a"); (Node.Untyped "b")];
            explicit_ret = None; body = ([], (Some (Node.Derive_expr [])))}))
    found: (Node.Statement
       (Node.Decl_stmt
          Node.Decl {
            tags =
            [(Node.Tag_call
                (Node.Decl_call (
                   (Node.Relational_expr
                      (Node.Relational_val
                         (Node.Additive_val
                            (Node.Multiplicative_val
                               (Node.Unary_val (Node.Ident "derive")))))),
                   [(Node.Positional
                       (Node.Relational_expr
                          (Node.Relational_val
                             (Node.Additive_val
                                (Node.Multiplicative_val
                                   (Node.Unary_call
                                      (Node.Decl_call (
                                         (Node.Relational_expr
                                            (Node.Relational_val
                                               (Node.Additive_val
                                                  (Node.Multiplicative_val
                                                     (Node.Unary_val
                                                        (Node.Ident
                                                           "my_derive_with_params")))))),
                                         [(Node.Positional
                                             (Node.Relational_expr
                                                (Node.Relational_val
                                                   (Node.Additive_val
                                                      (Node.Multiplicative_val
                                                         (Node.Unary_val
                                                            (Node.String "a")))))));
                                           (Node.Positional
                                              (Node.Relational_expr
                                                 (Node.Relational_val
                                                    (Node.Additive_val
                                                       (Node.Multiplicative_val
                                                          (Node.Unary_val
                                                             (Node.String "b")))))))
                                           ]
                                         ))))))))
                     ]
                   )))
              ];
            name = "some_struct"; params = []; explicit_ret = None;
            body = ([], (Some (Node.Struct_expr ([], None))))}))
    File: currying_functions
    found: (Node.Statement
       (Node.Decl_stmt
          Node.Curry_decl {tags = []; name = "mul_5"; curried = "mul";
            input =
            [(Node.Relational_expr
                (Node.Relational_val
                   (Node.Additive_val
                      (Node.Multiplicative_val (Node.Unary_val (Node.Int 5))))))
              ]}))
    found: (Node.Statement
       (Node.Decl_stmt
          Node.Curry_decl {tags = []; name = "just_15"; curried = "mul";
            input =
            [(Node.Relational_expr
                (Node.Relational_val
                   (Node.Additive_val
                      (Node.Multiplicative_val (Node.Unary_val (Node.Int 5))))));
              (Node.Relational_expr
                 (Node.Relational_val
                    (Node.Additive_val
                       (Node.Multiplicative_val (Node.Unary_val (Node.Int 3))))))
              ]}))
    File: open_statements
    found: (Node.Statement (Node.Open_stmt { Node.mods = ["MyModule"]; elements = [] }))
    found: (Node.Statement
       (Node.Open_stmt { Node.mods = ["MyModule"; "SomeMod"]; elements = [] }))
    found: (Node.Statement
       (Node.Open_stmt
          { Node.mods = ["Panic"];
            elements = [{ Node.path = ["panic"]; alias = None }] }))
    found: (Node.Statement
       (Node.Open_stmt
          { Node.mods = ["Assert"];
            elements =
            [{ Node.path = ["assert"]; alias = None };
              { Node.path = ["Static"; "assert_static"]; alias = None };
              { Node.path = ["assert_eq"]; alias = (Some "is_eq") }]
            }))
    File: tags_and_macros
    found: (Node.Statement
       (Node.Decl_stmt
          Node.Decl {
            tags =
            [(Node.Tag_name "recusive");
              (Node.Tag_call
                 (Node.Decl_call (
                    (Node.Relational_expr
                       (Node.Relational_val
                          (Node.Additive_val
                             (Node.Multiplicative_val
                                (Node.Unary_val (Node.Ident "derive")))))),
                    [(Node.Positional
                        (Node.Relational_expr
                           (Node.Relational_val
                              (Node.Additive_val
                                 (Node.Multiplicative_val
                                    (Node.Unary_val (Node.Ident "Show")))))));
                      (Node.Positional
                         (Node.Relational_expr
                            (Node.Relational_val
                               (Node.Additive_val
                                  (Node.Multiplicative_val
                                     (Node.Unary_val (Node.Ident "PrettyPrint")))))))
                      ]
                    )))
              ];
            name = "my_type"; params = []; explicit_ret = None;
            body =
            ([],
             (Some (Node.Relational_expr
                      (Node.Relational_val
                         (Node.Additive_val
                            (Node.Multiplicative_val
                               (Node.Unary_val (Node.String "ahhh"))))))))}))
    |}]
