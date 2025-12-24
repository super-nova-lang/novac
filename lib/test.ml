let test_lexer (name, content) =
  Format.printf "File: %s\n" name;
  let tokens = Lexer.lex "test.nova" content in
  List.iter (fun (x, _) -> Format.printf "found: %s\n" (Token.show x)) tokens
;;

let test_parser (name, content) =
  Format.printf "File: %s\n" name;
  let tokens = Lexer.lex "test.nova" content in
  let nodes = Parser.parse (Parser.create tokens) in
  List.iter (fun x -> Format.printf "found: %s\n" (Ast.show x)) nodes
;;

let test_codegen (name, content) =
  Format.printf "File: %s\n" name;
  let tokens = Lexer.lex "test.nova" content in
  let nodes = Parser.parse (Parser.create tokens) in
  List.iter Codegen.codegen nodes;
  Llvm.dump_module Codegen.the_module
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
    File: codegen_test
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
    found: (Token.Ident "main")
    found: Token.Double_colon
    found: Token.Open_paren
    found: Token.Close_paren
    found: Token.Eql
    found: (Token.Ident "add")
    found: Token.Open_paren
    found: (Token.Number 5)
    found: Token.Comma
    found: (Token.Number 3)
    found: Token.Close_paren
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
    File: ffi_example
    found: Token.Let
    found: (Token.Ident "printf")
    found: Token.Double_colon
    found: Token.Import
    found: Token.Back_arrow
    found: (Token.String "c")
    found: Token.Comma
    found: (Token.String "printf")
    found: Token.Let
    found: (Token.Ident "main")
    found: Token.Double_colon
    found: Token.Open_paren
    found: Token.Close_paren
    found: Token.Eql
    found: Token.Open_brack
    found: (Token.Ident "printf")
    found: Token.Open_paren
    found: (Token.String "Hello world! %d\n")
    found: Token.Comma
    found: (Token.Number 32)
    found: Token.Close_paren
    found: Token.Close_brack
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
    found: (Ast.Statement
       (Ast.Decl_stmt
          Ast.Decl {tags = []; name = "add";
            params =
            [(Ast.Typed ("a", (Ast.User "i32")));
              (Ast.Typed ("b", (Ast.User "i32")))];
            explicit_ret = None;
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
            params = [(Ast.Untyped "a"); (Ast.Untyped "b")];
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
            params = [(Ast.Untyped "a"); (Ast.Untyped "b")];
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
            params = [(Ast.Untyped "a"); (Ast.Untyped "b"); (Ast.Untyped "c")];
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
                                                               (Ast.Ident "res1")))),
                                                      (Ast.Multiplicative_val
                                                         (Ast.Unary_val
                                                            (Ast.Int 55)))
                                                      ))))))
                                       )))))))})
               ],
             (Some (Ast.Relational_expr
                      (Ast.Relational_val
                         (Ast.Additive_val
                            (Ast.Multiplicative_val
                               (Ast.Unary_val (Ast.Ident "res3"))))))))}))
    File: codegen_test
    found: (Ast.Statement
       (Ast.Decl_stmt
          Ast.Decl {tags = []; name = "add";
            params =
            [(Ast.Typed ("a", (Ast.User "i32")));
              (Ast.Typed ("b", (Ast.User "i32")))];
            explicit_ret = None;
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
                                                 (Ast.Unary_val (Ast.Ident "add")))))),
                                     [(Ast.Positional
                                         (Ast.Relational_expr
                                            (Ast.Relational_val
                                               (Ast.Additive_val
                                                  (Ast.Multiplicative_val
                                                     (Ast.Unary_val (Ast.Int 5)))))));
                                       (Ast.Positional
                                          (Ast.Relational_expr
                                             (Ast.Relational_val
                                                (Ast.Additive_val
                                                   (Ast.Multiplicative_val
                                                      (Ast.Unary_val (Ast.Int 3)))))))
                                       ]
                                     )))))))))}))
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
                                                         )))))))}))
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
                                                                        "Hello, my name is {} and I am a {}")))))));
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
                                                        (Ast.String "Ashton")))))));
                                       (Ast.Positional
                                          (Ast.Relational_expr
                                             (Ast.Relational_val
                                                (Ast.Additive_val
                                                   (Ast.Multiplicative_val
                                                      (Ast.Unary_val (Ast.Int 19)))))));
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
                                                                        (
                                                                        Ast.Unary_member (
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
                                                                 (Ast.Relational_expr
                                                                    (Ast.Relational_val
                                                                       (Ast.Additive_val
                                                                        (Ast.Multiplicative_val
                                                                        (Ast.Unary_val
                                                                        (Ast.String
                                                                        "nova")))))))
                                                              ]
                                                            ))))))))
                                       ]
                                     )))))))))}))
    found: (Ast.Statement
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
                                                        (Ast.String "Joe Shmoe")))))));
                                       (Ast.Positional
                                          (Ast.Relational_expr
                                             (Ast.Relational_val
                                                (Ast.Additive_val
                                                   (Ast.Multiplicative_val
                                                      (Ast.Unary_val (Ast.Int 37)))))));
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
                                                                        (
                                                                        Ast.Unary_val
                                                                        (Ast.Implicit_member
                                                                        "sales_rep")))))),
                                                            []))))))))
                                       ]
                                     )))))))))}))
    found: (Ast.Statement
       (Ast.Decl_stmt
          Ast.Decl {tags = []; name = "my_printf";
            params = [(Ast.Untyped "fmt"); (Ast.Variadic "args")];
            explicit_ret = None; body = ([], (Some (Ast.Macro_expr [])))}))
    found: (Ast.Statement
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
                                        (Ast.Unary_val (Ast.Ident "my_printf")))))),
                            [(Ast.Positional
                                (Ast.Relational_expr
                                   (Ast.Relational_val
                                      (Ast.Additive_val
                                         (Ast.Multiplicative_val
                                            (Ast.Unary_val
                                               (Ast.String "Hello world!")))))))
                              ]
                            )))))))))
    found: (Ast.Statement
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
                                        (Ast.Unary_val (Ast.Ident "my_printf")))))),
                            [(Ast.Positional
                                (Ast.Relational_expr
                                   (Ast.Relational_val
                                      (Ast.Additive_val
                                         (Ast.Multiplicative_val
                                            (Ast.Unary_val
                                               (Ast.String "This is {}")))))));
                              (Ast.Positional
                                 (Ast.Relational_expr
                                    (Ast.Relational_val
                                       (Ast.Additive_val
                                          (Ast.Multiplicative_val
                                             (Ast.Unary_val (Ast.String "nova")))))))
                              ]
                            )))))))))
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
    File: currying_functions
    found: (Ast.Statement
       (Ast.Decl_stmt
          Ast.Curry_decl {tags = []; name = "mul_5"; curried = "mul";
            input =
            [(Ast.Relational_expr
                (Ast.Relational_val
                   (Ast.Additive_val
                      (Ast.Multiplicative_val (Ast.Unary_val (Ast.Int 5))))))
              ]}))
    found: (Ast.Statement
       (Ast.Decl_stmt
          Ast.Curry_decl {tags = []; name = "just_15"; curried = "mul";
            input =
            [(Ast.Relational_expr
                (Ast.Relational_val
                   (Ast.Additive_val
                      (Ast.Multiplicative_val (Ast.Unary_val (Ast.Int 5))))));
              (Ast.Relational_expr
                 (Ast.Relational_val
                    (Ast.Additive_val
                       (Ast.Multiplicative_val (Ast.Unary_val (Ast.Int 3))))))
              ]}))
    File: ffi_example
    found: (Ast.Statement
       (Ast.Decl_stmt
          Ast.Import_decl {name = "printf"; calling_conf = "c";
            link_name = "printf"}))
    found: (Ast.Statement
       (Ast.Decl_stmt
          Ast.Decl {tags = []; name = "main"; params = []; explicit_ret = None;
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
                                                    (Ast.Ident "printf")))))),
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
                                     )))))))))}))
    File: open_statements
    found: (Ast.Statement (Ast.Open_stmt { Ast.mods = ["MyModule"]; elements = [] }))
    found: (Ast.Statement
       (Ast.Open_stmt { Ast.mods = ["MyModule"; "SomeMod"]; elements = [] }))
    found: (Ast.Statement
       (Ast.Open_stmt
          { Ast.mods = ["Panic"];
            elements = [{ Ast.path = ["panic"]; alias = None }] }))
    found: (Ast.Statement
       (Ast.Open_stmt
          { Ast.mods = ["Assert"];
            elements =
            [{ Ast.path = ["assert"]; alias = None };
              { Ast.path = ["Static"; "assert_static"]; alias = None };
              { Ast.path = ["assert_eq"]; alias = (Some "is_eq") }]
            }))
    File: tags_and_macros
    found: (Ast.Statement
       (Ast.Decl_stmt
          Ast.Decl {
            tags =
            [(Ast.Tag_name "recusive");
              (Ast.Tag_call
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
            name = "my_type"; params = []; explicit_ret = None;
            body =
            ([],
             (Some (Ast.Relational_expr
                      (Ast.Relational_val
                         (Ast.Additive_val
                            (Ast.Multiplicative_val
                               (Ast.Unary_val (Ast.String "ahhh"))))))))}))
    |}]
;;

[@@@ocamlformat "disable"]
let%expect_test "codegen" =
  List.iter test_codegen Nova_tests.all;
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)
  ("Novac.Codegen.Error(\"Complex function body not implemented yet\")")
  Raised at Novac__Codegen.codegen_decl in file "lib/codegen.ml", line 137, characters 13-70
  Called from Novac__Codegen.codegen_stmt in file "lib/codegen.ml", line 157, characters 31-50
  Called from Stdlib__List.iter in file "list.ml", line 114, characters 12-15
  Called from Novac__Test.test_codegen in file "lib/test.ml", line 18, characters 2-33
  Called from Stdlib__List.iter in file "list.ml", line 114, characters 12-15
  Called from Novac__Test.(fun) in file "lib/test.ml", line 1182, characters 2-39
  Called from Ppx_expect_runtime__Test_block.Configured.dump_backtrace in file "runtime/test_block.ml", line 142, characters 10-28

  Trailing output
  ---------------
  File: basic_functions
  |}]
