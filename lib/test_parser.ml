let test_parser (name, content) =
  Format.printf "File: %s\n" name;
  let tokens = Lexer.lex "test.nova" content in
  let nodes = Parser.parse (Parser.create tokens) in
  List.iter (fun x -> Format.printf "found: %s\n" (Ast.show x)) nodes
;;

let%expect_test "parser" =
  List.iter test_parser Nova_tests.all;
  [%expect
    {|
    File: basic_functions
    found: (Ast.Statement
       (Ast.Decl_stmt
          Ast.Decl {tags = []; name = "add";
            params =
            [(Ast.Typed ("a", (Ast.User "i32")));
              (Ast.Typed ("b", (Ast.User "i32")))];
            explicit_ret = (Some (Ast.User "i32"));
            body =
            ([],
             (Some (Ast.Relational_expr
                      (Ast.Relational_val
                         (Ast.Add (
                            (Ast.Additive_val
                               (Ast.Multiplicative_val
                                  (Ast.Unary_val (Ast.Ident "a")))),
                            (Ast.Multiplicative_val
                               (Ast.Unary_val (Ast.Ident "b")))
                            ))))))}))
    found: (Ast.Statement
       (Ast.Decl_stmt
          Ast.Decl {tags = []; name = "sub";
            params =
            [(Ast.Typed ("a", (Ast.User "i32")));
              (Ast.Typed ("b", (Ast.User "i32")))];
            explicit_ret = (Some (Ast.User "i32"));
            body =
            ([],
             (Some (Ast.Relational_expr
                      (Ast.Relational_val
                         (Ast.Sub (
                            (Ast.Additive_val
                               (Ast.Multiplicative_val
                                  (Ast.Unary_val (Ast.Ident "a")))),
                            (Ast.Multiplicative_val
                               (Ast.Unary_val (Ast.Ident "b")))
                            ))))))}))
    found: (Ast.Statement
       (Ast.Decl_stmt
          Ast.Decl {tags = []; name = "mul";
            params =
            [(Ast.Typed ("a", (Ast.User "i32")));
              (Ast.Typed ("b", (Ast.User "i32")))];
            explicit_ret = (Some (Ast.User "i32"));
            body =
            ([],
             (Some (Ast.Relational_expr
                      (Ast.Relational_val
                         (Ast.Additive_val
                            (Ast.Mul (
                               (Ast.Multiplicative_val
                                  (Ast.Unary_val (Ast.Ident "a"))),
                               (Ast.Unary_val (Ast.Ident "b")))))))))}))
    found: (Ast.Statement
       (Ast.Decl_stmt
          Ast.Decl {tags = []; name = "my_complex_fn";
            params =
            [(Ast.Typed ("a", (Ast.User "i32")));
              (Ast.Typed ("b", (Ast.User "i32")));
              (Ast.Typed ("c", (Ast.User "i32")))];
            explicit_ret = (Some (Ast.User "i32"));
            body =
            ([(Ast.Decl_stmt
                 Ast.Decl {tags = []; name = "res1"; params = [];
                   explicit_ret = None;
                   body =
                   ([],
                    (Some (Ast.Relational_expr
                             (Ast.Relational_val
                                (Ast.Add (
                                   (Ast.Additive_val
                                      (Ast.Multiplicative_val
                                         (Ast.Unary_val (Ast.Ident "a")))),
                                   (Ast.Mul (
                                      (Ast.Multiplicative_val
                                         (Ast.Unary_val (Ast.Ident "b"))),
                                      (Ast.Unary_val (Ast.Int 2))))
                                   ))))))});
               (Ast.Decl_stmt
                  Ast.Decl {tags = []; name = "res2"; params = [];
                    explicit_ret = None;
                    body =
                    ([],
                     (Some (Ast.Relational_expr
                              (Ast.Relational_val
                                 (Ast.Additive_val
                                    (Ast.Div (
                                       (Ast.Multiplicative_val
                                          (Ast.Unary_val (Ast.Ident "b"))),
                                       (Ast.Unary_val (Ast.Ident "res1")))))))))});
               (Ast.Decl_stmt
                  Ast.Decl {tags = []; name = "res3"; params = [];
                    explicit_ret = None;
                    body =
                    ([],
                     (Some (Ast.Relational_expr
                              (Ast.Relational_val
                                 (Ast.Sub (
                                    (Ast.Additive_val
                                       (Ast.Pow (
                                          (Ast.Multiplicative_val
                                             (Ast.Unary_val (Ast.Ident "c"))),
                                          (Ast.Unary_val
                                             (Ast.Grouping
                                                (Ast.Relational_expr
                                                   (Ast.Relational_val
                                                      (Ast.Sub (
                                                         (Ast.Additive_val
                                                            (Ast.Multiplicative_val
                                                               (Ast.Unary_val
                                                                  (Ast.Ident
                                                                     "res1")))),
                                                         (Ast.Multiplicative_val
                                                            (Ast.Unary_val
                                                               (Ast.Int 55)))
                                                         ))))))
                                          ))),
                                    (Ast.Multiplicative_val
                                       (Ast.Unary_val (Ast.Ident "res2")))
                                    ))))))})
               ],
             (Some (Ast.Relational_expr
                      (Ast.Relational_val
                         (Ast.Additive_val
                            (Ast.Multiplicative_val
                               (Ast.Unary_val (Ast.Ident "res3"))))))))}))
    found: (Ast.Statement
       (Ast.Decl_stmt
          Ast.Decl {tags = []; name = "main"; params = []; explicit_ret = None;
            body =
            ([(Ast.Expression_stmt
                 (Ast.Relational_expr
                    (Ast.Relational_val
                       (Ast.Additive_val
                          (Ast.Multiplicative_val
                             (Ast.Unary_call
                                (Ast.Decl_call (
                                   (Ast.Relational_expr
                                      (Ast.Relational_val
                                         (Ast.Additive_val
                                            (Ast.Multiplicative_val
                                               (Ast.Unary_member (
                                                  (Ast.Unary_val
                                                     (Ast.Ident "std_c")),
                                                  "printf")))))),
                                   [(Ast.Positional
                                       (Ast.Relational_expr
                                          (Ast.Relational_val
                                             (Ast.Additive_val
                                                (Ast.Multiplicative_val
                                                   (Ast.Unary_val
                                                      (Ast.String
                                                         "add(6, 7) = %d\n")))))));
                                     (Ast.Positional
                                        (Ast.Relational_expr
                                           (Ast.Relational_val
                                              (Ast.Additive_val
                                                 (Ast.Multiplicative_val
                                                    (Ast.Unary_call
                                                       (Ast.Decl_call (
                                                          (Ast.Relational_expr
                                                             (Ast.Relational_val
                                                                (Ast.Additive_val
                                                                   (Ast.Multiplicative_val
                                                                      (Ast.Unary_val
                                                                        (Ast.Ident
                                                                        "add")))))),
                                                          [(Ast.Positional
                                                              (Ast.Relational_expr
                                                                 (Ast.Relational_val
                                                                    (Ast.Additive_val
                                                                       (Ast.Multiplicative_val
                                                                        (Ast.Unary_val
                                                                        (Ast.Int
                                                                        6)))))));
                                                            (Ast.Positional
                                                               (Ast.Relational_expr
                                                                  (Ast.Relational_val
                                                                     (Ast.Additive_val
                                                                        (
                                                                        Ast.Multiplicative_val
                                                                        (Ast.Unary_val
                                                                        (Ast.Int
                                                                        7)))))))
                                                            ]
                                                          ))))))))
                                     ]
                                   ))))))));
               (Ast.Expression_stmt
                  (Ast.Relational_expr
                     (Ast.Relational_val
                        (Ast.Additive_val
                           (Ast.Multiplicative_val
                              (Ast.Unary_call
                                 (Ast.Decl_call (
                                    (Ast.Relational_expr
                                       (Ast.Relational_val
                                          (Ast.Additive_val
                                             (Ast.Multiplicative_val
                                                (Ast.Unary_member (
                                                   (Ast.Unary_val
                                                      (Ast.Ident "std_c")),
                                                   "printf")))))),
                                    [(Ast.Positional
                                        (Ast.Relational_expr
                                           (Ast.Relational_val
                                              (Ast.Additive_val
                                                 (Ast.Multiplicative_val
                                                    (Ast.Unary_val
                                                       (Ast.String
                                                          "mul(1, 1) = %d\n")))))));
                                      (Ast.Positional
                                         (Ast.Relational_expr
                                            (Ast.Relational_val
                                               (Ast.Additive_val
                                                  (Ast.Multiplicative_val
                                                     (Ast.Unary_call
                                                        (Ast.Decl_call (
                                                           (Ast.Relational_expr
                                                              (Ast.Relational_val
                                                                 (Ast.Additive_val
                                                                    (Ast.Multiplicative_val
                                                                       (Ast.Unary_val
                                                                        (Ast.Ident
                                                                        "mul")))))),
                                                           [(Ast.Positional
                                                               (Ast.Relational_expr
                                                                  (Ast.Relational_val
                                                                     (Ast.Additive_val
                                                                        (
                                                                        Ast.Multiplicative_val
                                                                        (Ast.Unary_val
                                                                        (Ast.Int
                                                                        1)))))));
                                                             (Ast.Positional
                                                                (Ast.Relational_expr
                                                                   (Ast.Relational_val
                                                                      (Ast.Additive_val
                                                                        (Ast.Multiplicative_val
                                                                        (Ast.Unary_val
                                                                        (Ast.Int
                                                                        1)))))))
                                                             ]
                                                           ))))))))
                                      ]
                                    ))))))));
               (Ast.Expression_stmt
                  (Ast.Relational_expr
                     (Ast.Relational_val
                        (Ast.Additive_val
                           (Ast.Multiplicative_val
                              (Ast.Unary_call
                                 (Ast.Decl_call (
                                    (Ast.Relational_expr
                                       (Ast.Relational_val
                                          (Ast.Additive_val
                                             (Ast.Multiplicative_val
                                                (Ast.Unary_member (
                                                   (Ast.Unary_val
                                                      (Ast.Ident "std_c")),
                                                   "printf")))))),
                                    [(Ast.Positional
                                        (Ast.Relational_expr
                                           (Ast.Relational_val
                                              (Ast.Additive_val
                                                 (Ast.Multiplicative_val
                                                    (Ast.Unary_val
                                                       (Ast.String
                                                          "my_complex_fn(2, 1, 5) = %d\n")))))));
                                      (Ast.Positional
                                         (Ast.Relational_expr
                                            (Ast.Relational_val
                                               (Ast.Additive_val
                                                  (Ast.Multiplicative_val
                                                     (Ast.Unary_call
                                                        (Ast.Decl_call (
                                                           (Ast.Relational_expr
                                                              (Ast.Relational_val
                                                                 (Ast.Additive_val
                                                                    (Ast.Multiplicative_val
                                                                       (Ast.Unary_val
                                                                        (Ast.Ident
                                                                        "my_complex_fn")))))),
                                                           [(Ast.Positional
                                                               (Ast.Relational_expr
                                                                  (Ast.Relational_val
                                                                     (Ast.Additive_val
                                                                        (
                                                                        Ast.Multiplicative_val
                                                                        (Ast.Unary_val
                                                                        (Ast.Int
                                                                        2)))))));
                                                             (Ast.Positional
                                                                (Ast.Relational_expr
                                                                   (Ast.Relational_val
                                                                      (Ast.Additive_val
                                                                        (Ast.Multiplicative_val
                                                                        (Ast.Unary_val
                                                                        (Ast.Int
                                                                        1)))))));
                                                             (Ast.Positional
                                                                (Ast.Relational_expr
                                                                   (Ast.Relational_val
                                                                      (Ast.Additive_val
                                                                        (Ast.Multiplicative_val
                                                                        (Ast.Unary_val
                                                                        (Ast.Int
                                                                        5)))))))
                                                             ]
                                                           ))))))))
                                      ]
                                    ))))))))
               ],
             (Some (Ast.Relational_expr
                      (Ast.Relational_val
                         (Ast.Additive_val
                            (Ast.Multiplicative_val (Ast.Unary_val (Ast.Int 0))))))))}))
    File: complex_types
    found: (Ast.Statement
       (Ast.Decl_stmt
          Ast.Decl {
            tags =
            [(Ast.Tag_call
                (Ast.Decl_call (
                   (Ast.Relational_expr
                      (Ast.Relational_val
                         (Ast.Additive_val
                            (Ast.Multiplicative_val
                               (Ast.Unary_val (Ast.Ident "derive")))))),
                   [(Ast.Positional
                       (Ast.Relational_expr
                          (Ast.Relational_val
                             (Ast.Additive_val
                                (Ast.Multiplicative_val
                                   (Ast.Unary_val (Ast.Ident "PrettyPrint")))))))
                     ]
                   )))
              ];
            name = "Job";
            params =
            [(Ast.Untyped "salary");
              (Ast.OptionalUntyped ("language",
                 (Ast.Relational_expr
                    (Ast.Relational_val
                       (Ast.Additive_val
                          (Ast.Multiplicative_val
                             (Ast.Unary_val (Ast.String "c++"))))))
                 ))
              ];
            explicit_ret = None;
            body =
            ([],
             (Some (Ast.Enum_expr (
                      [("programmer",
                        (Some (Ast.Struct_body
                                 [("language", (Ast.User "string"), None)])));
                        ("other", (Some (Ast.Type_body (Ast.User "string"))));
                        ("sales_rep", None)],
                      (Some [(Ast.Statement
                                (Ast.Decl_stmt
                                   Ast.Decl {tags = []; name = "salary";
                                     params = [];
                                     explicit_ret = (Some (Ast.User "i32"));
                                     body = ([], None)}));
                              (Ast.Statement
                                 (Ast.Decl_stmt
                                    Ast.Decl {tags = []; name = "hourly";
                                      params =
                                      [(Ast.Untyped "self");
                                        (Ast.Untyped "hours")];
                                      explicit_ret = None;
                                      body =
                                      ([],
                                       (Some (Ast.Relational_expr
                                                (Ast.Relational_val
                                                   (Ast.Additive_val
                                                      (Ast.Div (
                                                         (Ast.Multiplicative_val
                                                            (Ast.Unary_member (
                                                               (Ast.Unary_val
                                                                  (Ast.Ident
                                                                     "self")),
                                                               "salary"))),
                                                         (Ast.Unary_val
                                                            (Ast.Ident "hours"))
                                                         )))))))}));
                              (Ast.Statement
                                 (Ast.Decl_stmt
                                    Ast.Decl {tags = []; name = "show";
                                      params = [(Ast.Untyped "self")];
                                      explicit_ret = None;
                                      body =
                                      ([],
                                       (Some (Ast.Match_expr
                                                ((Ast.Relational_expr
                                                    (Ast.Relational_val
                                                       (Ast.Additive_val
                                                          (Ast.Multiplicative_val
                                                             (Ast.Unary_val
                                                                (Ast.Ident "self")))))),
                                                 [((Ast.Single
                                                      (Ast.Relational_expr
                                                         (Ast.Relational_val
                                                            (Ast.Additive_val
                                                               (Ast.Multiplicative_val
                                                                  (Ast.Unary_val
                                                                     (Ast.Implicit_member
                                                                        "programmer"))))))),
                                                   None,
                                                   ([],
                                                    (Some (Ast.Relational_expr
                                                             (Ast.Relational_val
                                                                (Ast.Additive_val
                                                                   (Ast.Multiplicative_val
                                                                      (Ast.Unary_val
                                                                        (Ast.String
                                                                        "programmer")))))))));
                                                   ((Ast.Single
                                                       (Ast.Relational_expr
                                                          (Ast.Relational_val
                                                             (Ast.Additive_val
                                                                (Ast.Multiplicative_val
                                                                   (Ast.Unary_call
                                                                      (Ast.Decl_call (
                                                                        (Ast.Relational_expr
                                                                        (Ast.Relational_val
                                                                        (Ast.Additive_val
                                                                        (Ast.Multiplicative_val
                                                                        (Ast.Unary_val
                                                                        (Ast.Implicit_member
                                                                        "other")))))),
                                                                        [(Ast.Positional
                                                                        (Ast.Relational_expr
                                                                        (Ast.Relational_val
                                                                        (Ast.Additive_val
                                                                        (Ast.Multiplicative_val
                                                                        (Ast.Unary_val
                                                                        (Ast.Ident
                                                                        "s")))))))
                                                                        ])))))))),
                                                    None,
                                                    ([],
                                                     (Some (Ast.Relational_expr
                                                              (Ast.Relational_val
                                                                 (Ast.Additive_val
                                                                    (Ast.Multiplicative_val
                                                                       (Ast.Unary_val
                                                                        (Ast.Ident
                                                                        "s")))))))));
                                                   ((Ast.Single
                                                       (Ast.Relational_expr
                                                          (Ast.Relational_val
                                                             (Ast.Additive_val
                                                                (Ast.Multiplicative_val
                                                                   (Ast.Unary_val
                                                                      (Ast.Implicit_member
                                                                        "sales_rep"))))))),
                                                    None,
                                                    ([],
                                                     (Some (Ast.Relational_expr
                                                              (Ast.Relational_val
                                                                 (Ast.Additive_val
                                                                    (Ast.Multiplicative_val
                                                                       (Ast.Unary_val
                                                                        (Ast.String
                                                                        "sales rep")))))))))
                                                   ]))))}))
                              ])
                      ))))}))
    found: (Ast.Statement
       (Ast.Decl_stmt
          Ast.Decl {
            tags =
            [(Ast.Tag_call
                (Ast.Decl_call (
                   (Ast.Relational_expr
                      (Ast.Relational_val
                         (Ast.Additive_val
                            (Ast.Multiplicative_val
                               (Ast.Unary_val (Ast.Ident "derive")))))),
                   [(Ast.Positional
                       (Ast.Relational_expr
                          (Ast.Relational_val
                             (Ast.Additive_val
                                (Ast.Multiplicative_val
                                   (Ast.Unary_val (Ast.Ident "Show")))))));
                     (Ast.Positional
                        (Ast.Relational_expr
                           (Ast.Relational_val
                              (Ast.Additive_val
                                 (Ast.Multiplicative_val
                                    (Ast.Unary_val (Ast.Ident "PrettyPrint")))))))
                     ]
                   )))
              ];
            name = "Person";
            params =
            [(Ast.Untyped "name"); (Ast.Untyped "age"); (Ast.Untyped "job")];
            explicit_ret = None;
            body =
            ([],
             (Some (Ast.Struct_expr (
                      [("name", (Ast.User "string"),
                        (Some (Ast.Relational_expr
                                 (Ast.Relational_val
                                    (Ast.Additive_val
                                       (Ast.Multiplicative_val
                                          (Ast.Unary_val (Ast.Ident "name"))))))));
                        ("age", (Ast.User "u8"),
                         (Some (Ast.Relational_expr
                                  (Ast.Relational_val
                                     (Ast.Additive_val
                                        (Ast.Multiplicative_val
                                           (Ast.Unary_val (Ast.Ident "age"))))))));
                        ("job", (Ast.User "job"),
                         (Some (Ast.Relational_expr
                                  (Ast.Relational_val
                                     (Ast.Additive_val
                                        (Ast.Multiplicative_val
                                           (Ast.Unary_val (Ast.Ident "job"))))))))
                        ],
                      (Some [(Ast.Statement
                                (Ast.Decl_stmt
                                   Ast.Decl {tags = []; name = "introduce";
                                     params = [(Ast.Untyped "self")];
                                     explicit_ret = None;
                                     body =
                                     ([],
                                      (Some (Ast.Relational_expr
                                               (Ast.Relational_val
                                                  (Ast.Additive_val
                                                     (Ast.Multiplicative_val
                                                        (Ast.Unary_call
                                                           (Ast.Macro_call (
                                                              (Ast.Relational_expr
                                                                 (Ast.Relational_val
                                                                    (Ast.Additive_val
                                                                       (Ast.Multiplicative_val
                                                                        (Ast.Unary_val
                                                                        (Ast.Ident
                                                                        "println")))))),
                                                              [(Ast.Positional
                                                                  (Ast.Relational_expr
                                                                     (Ast.Relational_val
                                                                        (
                                                                        Ast.Additive_val
                                                                        (Ast.Multiplicative_val
                                                                        (Ast.Unary_val
                                                                        (Ast.String
                                                                        "Hello, my name is %s and I am a %s")))))));
                                                                (Ast.Positional
                                                                   (Ast.Relational_expr
                                                                      (Ast.Relational_val
                                                                        (Ast.Additive_val
                                                                        (Ast.Multiplicative_val
                                                                        (Ast.Unary_member (
                                                                        (Ast.Unary_val
                                                                        (Ast.Ident
                                                                        "self")),
                                                                        "name")))))));
                                                                (Ast.Positional
                                                                   (Ast.Relational_expr
                                                                      (Ast.Relational_val
                                                                        (Ast.Additive_val
                                                                        (Ast.Multiplicative_val
                                                                        (Ast.Unary_call
                                                                        (Ast.Decl_call (
                                                                        (Ast.Relational_expr
                                                                        (Ast.Relational_val
                                                                        (Ast.Additive_val
                                                                        (Ast.Multiplicative_val
                                                                        (Ast.Unary_member (
                                                                        (Ast.Unary_member (
                                                                        (Ast.Unary_val
                                                                        (Ast.Ident
                                                                        "self")),
                                                                        "job")),
                                                                        "show")))))),
                                                                        []))))))))
                                                                ]
                                                              )))))))))}))
                              ])
                      ))))}))
    found: (Ast.Statement
       (Ast.Decl_stmt
          Ast.Decl {tags = []; name = "my_printf";
            params = [(Ast.Untyped "fmt"); (Ast.Variadic "args")];
            explicit_ret = None; body = ([], (Some (Ast.Macro_expr [])))}))
    found: (Ast.Statement
       (Ast.Decl_stmt
          Ast.Decl {tags = []; name = "my_derive"; params = [(Ast.Untyped "tt")];
            explicit_ret = None; body = ([], (Some (Ast.Derive_expr [])))}))
    found: (Ast.Statement
       (Ast.Decl_stmt
          Ast.Decl {
            tags =
            [(Ast.Tag_call
                (Ast.Decl_call (
                   (Ast.Relational_expr
                      (Ast.Relational_val
                         (Ast.Additive_val
                            (Ast.Multiplicative_val
                               (Ast.Unary_val (Ast.Ident "derive")))))),
                   [(Ast.Positional
                       (Ast.Relational_expr
                          (Ast.Relational_val
                             (Ast.Additive_val
                                (Ast.Multiplicative_val
                                   (Ast.Unary_val (Ast.Ident "my_derive")))))))
                     ]
                   )))
              ];
            name = "some_struct"; params = []; explicit_ret = None;
            body = ([], (Some (Ast.Struct_expr ([], None))))}))
    found: (Ast.Statement
       (Ast.Decl_stmt
          Ast.Decl {tags = []; name = "my_derive_with_params";
            params = [(Ast.Untyped "tt"); (Ast.Untyped "a"); (Ast.Untyped "b")];
            explicit_ret = None; body = ([], (Some (Ast.Derive_expr [])))}))
    found: (Ast.Statement
       (Ast.Decl_stmt
          Ast.Decl {
            tags =
            [(Ast.Tag_call
                (Ast.Decl_call (
                   (Ast.Relational_expr
                      (Ast.Relational_val
                         (Ast.Additive_val
                            (Ast.Multiplicative_val
                               (Ast.Unary_val (Ast.Ident "derive")))))),
                   [(Ast.Positional
                       (Ast.Relational_expr
                          (Ast.Relational_val
                             (Ast.Additive_val
                                (Ast.Multiplicative_val
                                   (Ast.Unary_call
                                      (Ast.Decl_call (
                                         (Ast.Relational_expr
                                            (Ast.Relational_val
                                               (Ast.Additive_val
                                                  (Ast.Multiplicative_val
                                                     (Ast.Unary_val
                                                        (Ast.Ident
                                                           "my_derive_with_params")))))),
                                         [(Ast.Positional
                                             (Ast.Relational_expr
                                                (Ast.Relational_val
                                                   (Ast.Additive_val
                                                      (Ast.Multiplicative_val
                                                         (Ast.Unary_val
                                                            (Ast.String "a")))))));
                                           (Ast.Positional
                                              (Ast.Relational_expr
                                                 (Ast.Relational_val
                                                    (Ast.Additive_val
                                                       (Ast.Multiplicative_val
                                                          (Ast.Unary_val
                                                             (Ast.String "b")))))))
                                           ]
                                         ))))))))
                     ]
                   )))
              ];
            name = "some_struct"; params = []; explicit_ret = None;
            body = ([], (Some (Ast.Struct_expr ([], None))))}))
    found: (Ast.Statement
       (Ast.Decl_stmt
          Ast.Decl {tags = []; name = "main"; params = []; explicit_ret = None;
            body =
            ([(Ast.Decl_stmt
                 Ast.Decl {tags = []; name = "based_dev"; params = [];
                   explicit_ret = None;
                   body =
                   ([],
                    (Some (Ast.Relational_expr
                             (Ast.Relational_val
                                (Ast.Additive_val
                                   (Ast.Multiplicative_val
                                      (Ast.Unary_call
                                         (Ast.Decl_call (
                                            (Ast.Relational_expr
                                               (Ast.Relational_val
                                                  (Ast.Additive_val
                                                     (Ast.Multiplicative_val
                                                        (Ast.Unary_val
                                                           (Ast.Ident "Person")))))),
                                            [(Ast.Positional
                                                (Ast.Relational_expr
                                                   (Ast.Relational_val
                                                      (Ast.Additive_val
                                                         (Ast.Multiplicative_val
                                                            (Ast.Unary_val
                                                               (Ast.String
                                                                  "Ashton")))))));
                                              (Ast.Positional
                                                 (Ast.Relational_expr
                                                    (Ast.Relational_val
                                                       (Ast.Additive_val
                                                          (Ast.Multiplicative_val
                                                             (Ast.Unary_val
                                                                (Ast.Int 19)))))));
                                              (Ast.Positional
                                                 (Ast.Relational_expr
                                                    (Ast.Relational_val
                                                       (Ast.Additive_val
                                                          (Ast.Multiplicative_val
                                                             (Ast.Unary_call
                                                                (Ast.Decl_call (
                                                                   (Ast.Relational_expr
                                                                      (Ast.Relational_val
                                                                        (Ast.Additive_val
                                                                        (Ast.Multiplicative_val
                                                                        (Ast.Unary_member (
                                                                        (Ast.Unary_val
                                                                        (Ast.Ident
                                                                        "Job")),
                                                                        "programmer"
                                                                        )))))),
                                                                   [(Ast.Positional
                                                                       (Ast.Relational_expr
                                                                        (Ast.Relational_val
                                                                        (Ast.Additive_val
                                                                        (Ast.Multiplicative_val
                                                                        (Ast.Unary_val
                                                                        (Ast.Int
                                                                        1000)))))));
                                                                     (Ast.Positional
                                                                        (
                                                                        Ast.Relational_expr
                                                                        (Ast.Relational_val
                                                                        (Ast.Additive_val
                                                                        (Ast.Multiplicative_val
                                                                        (Ast.Unary_val
                                                                        (Ast.String
                                                                        "nova")))))))
                                                                     ]
                                                                   ))))))))
                                              ]
                                            )))))))))});
               (Ast.Expression_stmt
                  (Ast.Relational_expr
                     (Ast.Relational_val
                        (Ast.Additive_val
                           (Ast.Multiplicative_val
                              (Ast.Unary_call
                                 (Ast.Decl_call (
                                    (Ast.Relational_expr
                                       (Ast.Relational_val
                                          (Ast.Additive_val
                                             (Ast.Multiplicative_val
                                                (Ast.Unary_member (
                                                   (Ast.Unary_val
                                                      (Ast.Ident "based_dev")),
                                                   "introduce")))))),
                                    []))))))));
               (Ast.Expression_stmt
                  (Ast.Relational_expr
                     (Ast.Relational_val
                        (Ast.Additive_val
                           (Ast.Multiplicative_val
                              (Ast.Unary_call
                                 (Ast.Macro_call (
                                    (Ast.Relational_expr
                                       (Ast.Relational_val
                                          (Ast.Additive_val
                                             (Ast.Multiplicative_val
                                                (Ast.Unary_val
                                                   (Ast.Ident "println")))))),
                                    [(Ast.Positional
                                        (Ast.Relational_expr
                                           (Ast.Relational_val
                                              (Ast.Additive_val
                                                 (Ast.Multiplicative_val
                                                    (Ast.Unary_val
                                                       (Ast.String
                                                          "Hourly rate: %d")))))));
                                      (Ast.Positional
                                         (Ast.Relational_expr
                                            (Ast.Relational_val
                                               (Ast.Additive_val
                                                  (Ast.Multiplicative_val
                                                     (Ast.Unary_call
                                                        (Ast.Decl_call (
                                                           (Ast.Relational_expr
                                                              (Ast.Relational_val
                                                                 (Ast.Additive_val
                                                                    (Ast.Multiplicative_val
                                                                       (Ast.Unary_member (
                                                                        (Ast.Unary_member (
                                                                        (Ast.Unary_val
                                                                        (Ast.Ident
                                                                        "based_dev")),
                                                                        "job")),
                                                                        "hourly"
                                                                        )))))),
                                                           [(Ast.Positional
                                                               (Ast.Relational_expr
                                                                  (Ast.Relational_val
                                                                     (Ast.Additive_val
                                                                        (
                                                                        Ast.Multiplicative_val
                                                                        (Ast.Unary_val
                                                                        (Ast.Int
                                                                        40)))))))
                                                             ]
                                                           ))))))))
                                      ]
                                    ))))))));
               (Ast.Decl_stmt
                  Ast.Decl {tags = []; name = "prob_some_creep"; params = [];
                    explicit_ret = None;
                    body =
                    ([],
                     (Some (Ast.Relational_expr
                              (Ast.Relational_val
                                 (Ast.Additive_val
                                    (Ast.Multiplicative_val
                                       (Ast.Unary_call
                                          (Ast.Decl_call (
                                             (Ast.Relational_expr
                                                (Ast.Relational_val
                                                   (Ast.Additive_val
                                                      (Ast.Multiplicative_val
                                                         (Ast.Unary_val
                                                            (Ast.Ident "Person")))))),
                                             [(Ast.Positional
                                                 (Ast.Relational_expr
                                                    (Ast.Relational_val
                                                       (Ast.Additive_val
                                                          (Ast.Multiplicative_val
                                                             (Ast.Unary_val
                                                                (Ast.String
                                                                   "Joe Shmoe")))))));
                                               (Ast.Positional
                                                  (Ast.Relational_expr
                                                     (Ast.Relational_val
                                                        (Ast.Additive_val
                                                           (Ast.Multiplicative_val
                                                              (Ast.Unary_val
                                                                 (Ast.Int 37)))))));
                                               (Ast.Positional
                                                  (Ast.Relational_expr
                                                     (Ast.Relational_val
                                                        (Ast.Additive_val
                                                           (Ast.Multiplicative_val
                                                              (Ast.Unary_call
                                                                 (Ast.Decl_call (
                                                                    (Ast.Relational_expr
                                                                       (Ast.Relational_val
                                                                        (Ast.Additive_val
                                                                        (Ast.Multiplicative_val
                                                                        (Ast.Unary_val
                                                                        (Ast.Implicit_member
                                                                        "sales_rep")))))),
                                                                    []))))))))
                                               ]
                                             )))))))))});
               (Ast.Expression_stmt
                  (Ast.Relational_expr
                     (Ast.Relational_val
                        (Ast.Additive_val
                           (Ast.Multiplicative_val
                              (Ast.Unary_call
                                 (Ast.Decl_call (
                                    (Ast.Relational_expr
                                       (Ast.Relational_val
                                          (Ast.Additive_val
                                             (Ast.Multiplicative_val
                                                (Ast.Unary_member (
                                                   (Ast.Unary_val
                                                      (Ast.Ident
                                                         "prob_some_creep")),
                                                   "introduce")))))),
                                    []))))))))
               ],
             None)}))
    File: currying_functions
    found: (Ast.Statement
       (Ast.Decl_stmt
          Ast.Decl {tags = []; name = "mul";
            params =
            [(Ast.Typed ("a", (Ast.User "i32")));
              (Ast.Typed ("b", (Ast.User "i32")))];
            explicit_ret = (Some (Ast.User "i32"));
            body =
            ([],
             (Some (Ast.Relational_expr
                      (Ast.Relational_val
                         (Ast.Additive_val
                            (Ast.Mul (
                               (Ast.Multiplicative_val
                                  (Ast.Unary_val (Ast.Ident "a"))),
                               (Ast.Unary_val (Ast.Ident "b")))))))))}))
    found: (Ast.Statement
       (Ast.Decl_stmt
          Ast.Decl {tags = []; name = "add";
            params =
            [(Ast.Typed ("a", (Ast.User "i32")));
              (Ast.Typed ("b", (Ast.User "i32")))];
            explicit_ret = (Some (Ast.User "i32"));
            body =
            ([],
             (Some (Ast.Relational_expr
                      (Ast.Relational_val
                         (Ast.Add (
                            (Ast.Additive_val
                               (Ast.Multiplicative_val
                                  (Ast.Unary_val (Ast.Ident "a")))),
                            (Ast.Multiplicative_val
                               (Ast.Unary_val (Ast.Ident "b")))
                            ))))))}))
    found: (Ast.Statement
       (Ast.Decl_stmt
          Ast.Decl {tags = []; name = "main"; params = []; explicit_ret = None;
            body =
            ([(Ast.Decl_stmt
                 Ast.Decl {tags = []; name = "result"; params = [];
                   explicit_ret = None;
                   body =
                   ([],
                    (Some (Ast.Relational_expr
                             (Ast.Relational_val
                                (Ast.Additive_val
                                   (Ast.Multiplicative_val
                                      (Ast.Unary_call
                                         (Ast.Decl_call (
                                            (Ast.Relational_expr
                                               (Ast.Relational_val
                                                  (Ast.Additive_val
                                                     (Ast.Multiplicative_val
                                                        (Ast.Unary_val
                                                           (Ast.Ident "mul")))))),
                                            [(Ast.Positional
                                                (Ast.Relational_expr
                                                   (Ast.Relational_val
                                                      (Ast.Additive_val
                                                         (Ast.Multiplicative_val
                                                            (Ast.Unary_val
                                                               (Ast.Int 5)))))));
                                              (Ast.Positional
                                                 (Ast.Relational_expr
                                                    (Ast.Relational_val
                                                       (Ast.Additive_val
                                                          (Ast.Multiplicative_val
                                                             (Ast.Unary_val
                                                                (Ast.Int 3)))))))
                                              ]
                                            )))))))))});
               (Ast.Expression_stmt
                  (Ast.Relational_expr
                     (Ast.Relational_val
                        (Ast.Additive_val
                           (Ast.Multiplicative_val
                              (Ast.Unary_call
                                 (Ast.Decl_call (
                                    (Ast.Relational_expr
                                       (Ast.Relational_val
                                          (Ast.Additive_val
                                             (Ast.Multiplicative_val
                                                (Ast.Unary_member (
                                                   (Ast.Unary_val
                                                      (Ast.Ident "std_c")),
                                                   "printf")))))),
                                    [(Ast.Positional
                                        (Ast.Relational_expr
                                           (Ast.Relational_val
                                              (Ast.Additive_val
                                                 (Ast.Multiplicative_val
                                                    (Ast.Unary_val
                                                       (Ast.String "5 * 3 = %d\n")))))));
                                      (Ast.Positional
                                         (Ast.Relational_expr
                                            (Ast.Relational_val
                                               (Ast.Additive_val
                                                  (Ast.Multiplicative_val
                                                     (Ast.Unary_val
                                                        (Ast.Ident "result")))))))
                                      ]
                                    ))))))));
               (Ast.Expression_stmt
                  (Ast.Relational_expr
                     (Ast.Relational_val
                        (Ast.Additive_val
                           (Ast.Multiplicative_val
                              (Ast.Unary_call
                                 (Ast.Decl_call (
                                    (Ast.Relational_expr
                                       (Ast.Relational_val
                                          (Ast.Additive_val
                                             (Ast.Multiplicative_val
                                                (Ast.Unary_member (
                                                   (Ast.Unary_val
                                                      (Ast.Ident "std_c")),
                                                   "printf")))))),
                                    [(Ast.Positional
                                        (Ast.Relational_expr
                                           (Ast.Relational_val
                                              (Ast.Additive_val
                                                 (Ast.Multiplicative_val
                                                    (Ast.Unary_val
                                                       (Ast.String "2 + 3 = %d\n")))))));
                                      (Ast.Positional
                                         (Ast.Relational_expr
                                            (Ast.Relational_val
                                               (Ast.Additive_val
                                                  (Ast.Multiplicative_val
                                                     (Ast.Unary_call
                                                        (Ast.Decl_call (
                                                           (Ast.Relational_expr
                                                              (Ast.Relational_val
                                                                 (Ast.Additive_val
                                                                    (Ast.Multiplicative_val
                                                                       (Ast.Unary_val
                                                                        (Ast.Ident
                                                                        "add")))))),
                                                           [(Ast.Positional
                                                               (Ast.Relational_expr
                                                                  (Ast.Relational_val
                                                                     (Ast.Additive_val
                                                                        (
                                                                        Ast.Multiplicative_val
                                                                        (Ast.Unary_val
                                                                        (Ast.Int
                                                                        2)))))));
                                                             (Ast.Positional
                                                                (Ast.Relational_expr
                                                                   (Ast.Relational_val
                                                                      (Ast.Additive_val
                                                                        (Ast.Multiplicative_val
                                                                        (Ast.Unary_val
                                                                        (Ast.Int
                                                                        3)))))))
                                                             ]
                                                           ))))))))
                                      ]
                                    ))))))))
               ],
             (Some (Ast.Relational_expr
                      (Ast.Relational_val
                         (Ast.Additive_val
                            (Ast.Multiplicative_val (Ast.Unary_val (Ast.Int 0))))))))}))
    File: ffi_example
    found: (Ast.Statement
       (Ast.Decl_stmt
          Ast.Decl {tags = []; name = "main"; params = []; explicit_ret = None;
            body =
            ([(Ast.Expression_stmt
                 (Ast.Relational_expr
                    (Ast.Relational_val
                       (Ast.Additive_val
                          (Ast.Multiplicative_val
                             (Ast.Unary_call
                                (Ast.Decl_call (
                                   (Ast.Relational_expr
                                      (Ast.Relational_val
                                         (Ast.Additive_val
                                            (Ast.Multiplicative_val
                                               (Ast.Unary_member (
                                                  (Ast.Unary_val
                                                     (Ast.Ident "std_c")),
                                                  "printf")))))),
                                   [(Ast.Positional
                                       (Ast.Relational_expr
                                          (Ast.Relational_val
                                             (Ast.Additive_val
                                                (Ast.Multiplicative_val
                                                   (Ast.Unary_val
                                                      (Ast.String
                                                         "Hello world! %d\n")))))));
                                     (Ast.Positional
                                        (Ast.Relational_expr
                                           (Ast.Relational_val
                                              (Ast.Additive_val
                                                 (Ast.Multiplicative_val
                                                    (Ast.Unary_val (Ast.Int 32)))))))
                                     ]
                                   ))))))))
               ],
             (Some (Ast.Relational_expr
                      (Ast.Relational_val
                         (Ast.Additive_val
                            (Ast.Multiplicative_val (Ast.Unary_val (Ast.Int 0))))))))}))
    File: module_test
    found: (Ast.Statement
       (Ast.Decl_stmt
          Ast.Module_decl {name = "Math";
            exports =
            [(Ast.Export_ident "add"); (Ast.Export_rename ("sub", "subtract"))];
            body =
            [(Ast.Statement
                (Ast.Decl_stmt
                   Ast.Decl {tags = []; name = "add";
                     params =
                     [(Ast.Typed ("a", (Ast.User "i32")));
                       (Ast.Typed ("b", (Ast.User "i32")))];
                     explicit_ret = (Some (Ast.User "i32"));
                     body =
                     ([],
                      (Some (Ast.Relational_expr
                               (Ast.Relational_val
                                  (Ast.Add (
                                     (Ast.Additive_val
                                        (Ast.Multiplicative_val
                                           (Ast.Unary_val (Ast.Ident "a")))),
                                     (Ast.Multiplicative_val
                                        (Ast.Unary_val (Ast.Ident "b")))
                                     ))))))}));
              (Ast.Statement
                 (Ast.Decl_stmt
                    Ast.Decl {tags = []; name = "sub";
                      params =
                      [(Ast.Typed ("a", (Ast.User "i32")));
                        (Ast.Typed ("b", (Ast.User "i32")))];
                      explicit_ret = (Some (Ast.User "i32"));
                      body =
                      ([],
                       (Some (Ast.Relational_expr
                                (Ast.Relational_val
                                   (Ast.Sub (
                                      (Ast.Additive_val
                                         (Ast.Multiplicative_val
                                            (Ast.Unary_val (Ast.Ident "a")))),
                                      (Ast.Multiplicative_val
                                         (Ast.Unary_val (Ast.Ident "b")))
                                      ))))))}))
              ]}))
    found: (Ast.Statement
       (Ast.Decl_stmt
          Ast.Decl {tags = []; name = "main"; params = []; explicit_ret = None;
            body =
            ([(Ast.Decl_stmt
                 Ast.Decl {tags = []; name = "x"; params = [];
                   explicit_ret = None;
                   body =
                   ([],
                    (Some (Ast.Relational_expr
                             (Ast.Relational_val
                                (Ast.Additive_val
                                   (Ast.Multiplicative_val
                                      (Ast.Unary_call
                                         (Ast.Decl_call (
                                            (Ast.Relational_expr
                                               (Ast.Relational_val
                                                  (Ast.Additive_val
                                                     (Ast.Multiplicative_val
                                                        (Ast.Unary_member (
                                                           (Ast.Unary_val
                                                              (Ast.Ident "Math")),
                                                           "add")))))),
                                            [(Ast.Positional
                                                (Ast.Relational_expr
                                                   (Ast.Relational_val
                                                      (Ast.Additive_val
                                                         (Ast.Multiplicative_val
                                                            (Ast.Unary_val
                                                               (Ast.Int 10)))))));
                                              (Ast.Positional
                                                 (Ast.Relational_expr
                                                    (Ast.Relational_val
                                                       (Ast.Additive_val
                                                          (Ast.Multiplicative_val
                                                             (Ast.Unary_val
                                                                (Ast.Int 5)))))))
                                              ]
                                            )))))))))});
               (Ast.Decl_stmt
                  Ast.Decl {tags = []; name = "y"; params = [];
                    explicit_ret = None;
                    body =
                    ([],
                     (Some (Ast.Relational_expr
                              (Ast.Relational_val
                                 (Ast.Additive_val
                                    (Ast.Multiplicative_val
                                       (Ast.Unary_call
                                          (Ast.Decl_call (
                                             (Ast.Relational_expr
                                                (Ast.Relational_val
                                                   (Ast.Additive_val
                                                      (Ast.Multiplicative_val
                                                         (Ast.Unary_member (
                                                            (Ast.Unary_val
                                                               (Ast.Ident "Math")),
                                                            "sub")))))),
                                             [(Ast.Positional
                                                 (Ast.Relational_expr
                                                    (Ast.Relational_val
                                                       (Ast.Additive_val
                                                          (Ast.Multiplicative_val
                                                             (Ast.Unary_val
                                                                (Ast.Int 10)))))));
                                               (Ast.Positional
                                                  (Ast.Relational_expr
                                                     (Ast.Relational_val
                                                        (Ast.Additive_val
                                                           (Ast.Multiplicative_val
                                                              (Ast.Unary_val
                                                                 (Ast.Int 5)))))))
                                               ]
                                             )))))))))});
               (Ast.Expression_stmt
                  (Ast.Relational_expr
                     (Ast.Relational_val
                        (Ast.Additive_val
                           (Ast.Multiplicative_val
                              (Ast.Unary_call
                                 (Ast.Decl_call (
                                    (Ast.Relational_expr
                                       (Ast.Relational_val
                                          (Ast.Additive_val
                                             (Ast.Multiplicative_val
                                                (Ast.Unary_member (
                                                   (Ast.Unary_val
                                                      (Ast.Ident "std_c")),
                                                   "printf")))))),
                                    [(Ast.Positional
                                        (Ast.Relational_expr
                                           (Ast.Relational_val
                                              (Ast.Additive_val
                                                 (Ast.Multiplicative_val
                                                    (Ast.Unary_val
                                                       (Ast.String "%d\n")))))));
                                      (Ast.Positional
                                         (Ast.Relational_expr
                                            (Ast.Relational_val
                                               (Ast.Additive_val
                                                  (Ast.Multiplicative_val
                                                     (Ast.Unary_val
                                                        (Ast.Ident "x")))))))
                                      ]
                                    ))))))));
               (Ast.Expression_stmt
                  (Ast.Relational_expr
                     (Ast.Relational_val
                        (Ast.Additive_val
                           (Ast.Multiplicative_val
                              (Ast.Unary_call
                                 (Ast.Decl_call (
                                    (Ast.Relational_expr
                                       (Ast.Relational_val
                                          (Ast.Additive_val
                                             (Ast.Multiplicative_val
                                                (Ast.Unary_member (
                                                   (Ast.Unary_val
                                                      (Ast.Ident "std_c")),
                                                   "printf")))))),
                                    [(Ast.Positional
                                        (Ast.Relational_expr
                                           (Ast.Relational_val
                                              (Ast.Additive_val
                                                 (Ast.Multiplicative_val
                                                    (Ast.Unary_val
                                                       (Ast.String "%d\n")))))));
                                      (Ast.Positional
                                         (Ast.Relational_expr
                                            (Ast.Relational_val
                                               (Ast.Additive_val
                                                  (Ast.Multiplicative_val
                                                     (Ast.Unary_val
                                                        (Ast.Ident "y")))))))
                                      ]
                                    ))))))))
               ],
             (Some (Ast.Relational_expr
                      (Ast.Relational_val
                         (Ast.Add (
                            (Ast.Additive_val
                               (Ast.Multiplicative_val
                                  (Ast.Unary_val (Ast.Ident "x")))),
                            (Ast.Multiplicative_val
                               (Ast.Unary_val (Ast.Ident "y")))
                            ))))))}))
    File: open_statements
    found: (Ast.Statement (Ast.Open_stmt { Ast.mods = ["std"]; elements = [] }))
    found: (Ast.Statement
       (Ast.Decl_stmt
          Ast.Decl {tags = []; name = "my_func"; params = [];
            explicit_ret = (Some (Ast.User "i32"));
            body =
            ([],
             (Some (Ast.Relational_expr
                      (Ast.Relational_val
                         (Ast.Additive_val
                            (Ast.Multiplicative_val (Ast.Unary_val (Ast.Int 123))))))))}))
    found: (Ast.Statement
       (Ast.Decl_stmt
          Ast.Decl {tags = []; name = "main"; params = []; explicit_ret = None;
            body =
            ([(Ast.Decl_stmt
                 Ast.Decl {tags = []; name = "result"; params = [];
                   explicit_ret = None;
                   body =
                   ([],
                    (Some (Ast.Relational_expr
                             (Ast.Relational_val
                                (Ast.Additive_val
                                   (Ast.Multiplicative_val
                                      (Ast.Unary_call
                                         (Ast.Decl_call (
                                            (Ast.Relational_expr
                                               (Ast.Relational_val
                                                  (Ast.Additive_val
                                                     (Ast.Multiplicative_val
                                                        (Ast.Unary_val
                                                           (Ast.Ident "my_func")))))),
                                            [])))))))))});
               (Ast.Expression_stmt
                  (Ast.Relational_expr
                     (Ast.Relational_val
                        (Ast.Additive_val
                           (Ast.Multiplicative_val
                              (Ast.Unary_call
                                 (Ast.Decl_call (
                                    (Ast.Relational_expr
                                       (Ast.Relational_val
                                          (Ast.Additive_val
                                             (Ast.Multiplicative_val
                                                (Ast.Unary_member (
                                                   (Ast.Unary_val
                                                      (Ast.Ident "std_c")),
                                                   "printf")))))),
                                    [(Ast.Positional
                                        (Ast.Relational_expr
                                           (Ast.Relational_val
                                              (Ast.Additive_val
                                                 (Ast.Multiplicative_val
                                                    (Ast.Unary_val
                                                       (Ast.String "Result: %d\n")))))));
                                      (Ast.Positional
                                         (Ast.Relational_expr
                                            (Ast.Relational_val
                                               (Ast.Additive_val
                                                  (Ast.Multiplicative_val
                                                     (Ast.Unary_val
                                                        (Ast.Ident "result")))))))
                                      ]
                                    ))))))))
               ],
             (Some (Ast.Relational_expr
                      (Ast.Relational_val
                         (Ast.Additive_val
                            (Ast.Multiplicative_val (Ast.Unary_val (Ast.Int 0))))))))}))
    File: stdlib_test
    found: (Ast.Statement
       (Ast.Decl_stmt
          Ast.Decl {tags = []; name = "main"; params = []; explicit_ret = None;
            body =
            ([(Ast.Decl_stmt
                 Ast.Decl {tags = []; name = "result"; params = [];
                   explicit_ret = None;
                   body =
                   ([],
                    (Some (Ast.Relational_expr
                             (Ast.Relational_val
                                (Ast.Additive_val
                                   (Ast.Multiplicative_val
                                      (Ast.Unary_call
                                         (Ast.Decl_call (
                                            (Ast.Relational_expr
                                               (Ast.Relational_val
                                                  (Ast.Additive_val
                                                     (Ast.Multiplicative_val
                                                        (Ast.Unary_member (
                                                           (Ast.Unary_val
                                                              (Ast.Ident
                                                                 "std_math")),
                                                           "square")))))),
                                            [(Ast.Positional
                                                (Ast.Relational_expr
                                                   (Ast.Relational_val
                                                      (Ast.Additive_val
                                                         (Ast.Multiplicative_val
                                                            (Ast.Unary_val
                                                               (Ast.Int 5)))))))
                                              ]
                                            )))))))))});
               (Ast.Expression_stmt
                  (Ast.Relational_expr
                     (Ast.Relational_val
                        (Ast.Additive_val
                           (Ast.Multiplicative_val
                              (Ast.Unary_call
                                 (Ast.Decl_call (
                                    (Ast.Relational_expr
                                       (Ast.Relational_val
                                          (Ast.Additive_val
                                             (Ast.Multiplicative_val
                                                (Ast.Unary_member (
                                                   (Ast.Unary_val
                                                      (Ast.Ident "std_c")),
                                                   "printf")))))),
                                    [(Ast.Positional
                                        (Ast.Relational_expr
                                           (Ast.Relational_val
                                              (Ast.Additive_val
                                                 (Ast.Multiplicative_val
                                                    (Ast.Unary_val
                                                       (Ast.String
                                                          "Square of 5: %d\n")))))));
                                      (Ast.Positional
                                         (Ast.Relational_expr
                                            (Ast.Relational_val
                                               (Ast.Additive_val
                                                  (Ast.Multiplicative_val
                                                     (Ast.Unary_val
                                                        (Ast.Ident "result")))))))
                                      ]
                                    ))))))))
               ],
             (Some (Ast.Relational_expr
                      (Ast.Relational_val
                         (Ast.Additive_val
                            (Ast.Multiplicative_val
                               (Ast.Unary_val (Ast.Ident "result"))))))))}))
    File: tags_and_macros
    found: (Ast.Statement
       (Ast.Decl_stmt
          Ast.Decl {tags = [(Ast.Tag_name "no_mangle")]; name = "my_func";
            params = []; explicit_ret = (Some (Ast.User "i32"));
            body =
            ([],
             (Some (Ast.Relational_expr
                      (Ast.Relational_val
                         (Ast.Additive_val
                            (Ast.Multiplicative_val (Ast.Unary_val (Ast.Int 42))))))))}))
    found: (Ast.Statement
       (Ast.Decl_stmt
          Ast.Decl {tags = []; name = "main"; params = []; explicit_ret = None;
            body =
            ([(Ast.Expression_stmt
                 (Ast.Relational_expr
                    (Ast.Relational_val
                       (Ast.Additive_val
                          (Ast.Multiplicative_val
                             (Ast.Unary_call
                                (Ast.Decl_call (
                                   (Ast.Relational_expr
                                      (Ast.Relational_val
                                         (Ast.Additive_val
                                            (Ast.Multiplicative_val
                                               (Ast.Unary_member (
                                                  (Ast.Unary_val
                                                     (Ast.Ident "std_c")),
                                                  "printf")))))),
                                   [(Ast.Positional
                                       (Ast.Relational_expr
                                          (Ast.Relational_val
                                             (Ast.Additive_val
                                                (Ast.Multiplicative_val
                                                   (Ast.Unary_val
                                                      (Ast.String
                                                         "Tagged function result: %d\n")))))));
                                     (Ast.Positional
                                        (Ast.Relational_expr
                                           (Ast.Relational_val
                                              (Ast.Additive_val
                                                 (Ast.Multiplicative_val
                                                    (Ast.Unary_call
                                                       (Ast.Decl_call (
                                                          (Ast.Relational_expr
                                                             (Ast.Relational_val
                                                                (Ast.Additive_val
                                                                   (Ast.Multiplicative_val
                                                                      (Ast.Unary_val
                                                                        (Ast.Ident
                                                                        "my_func")))))),
                                                          []))))))))
                                     ]
                                   ))))))))
               ],
             (Some (Ast.Relational_expr
                      (Ast.Relational_val
                         (Ast.Additive_val
                            (Ast.Multiplicative_val (Ast.Unary_val (Ast.Int 0))))))))}))
    |}]
;;
