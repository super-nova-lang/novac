let test_lexer (name, content) =
  Format.printf "File: %s\n" name;
  let tokens = Lexer.lex "test.nova" content in
  List.iter (fun (x, _) -> Format.printf "found: %s\n" (Token.show x)) tokens
;;

let%expect_test "lexer" =
  List.iter test_lexer Nova_tests.all;
  [%expect
    {|
    File: basic_functions
    found: Token.Let
    found: (Token.Ident "add")
    found: Token.Double_colon
    found: Token.Open_paren
    found: (Token.Ident "a")
    found: Token.Colon
    found: (Token.Ident "i32")
    found: Token.Comma
    found: (Token.Ident "b")
    found: Token.Colon
    found: (Token.Ident "i32")
    found: Token.Close_paren
    found: Token.Skinny_arrow
    found: (Token.Ident "i32")
    found: Token.Eql
    found: Token.Open_brack
    found: (Token.Ident "a")
    found: Token.Plus
    found: (Token.Ident "b")
    found: Token.Close_brack
    found: Token.Let
    found: (Token.Ident "sub")
    found: Token.Double_colon
    found: Token.Open_paren
    found: (Token.Ident "a")
    found: Token.Colon
    found: (Token.Ident "i32")
    found: Token.Comma
    found: (Token.Ident "b")
    found: Token.Colon
    found: (Token.Ident "i32")
    found: Token.Close_paren
    found: Token.Skinny_arrow
    found: (Token.Ident "i32")
    found: Token.Eql
    found: Token.Open_brack
    found: (Token.Ident "a")
    found: Token.Dash
    found: (Token.Ident "b")
    found: Token.Close_brack
    found: Token.Let
    found: (Token.Ident "mul")
    found: Token.Double_colon
    found: Token.Open_paren
    found: (Token.Ident "a")
    found: Token.Colon
    found: (Token.Ident "i32")
    found: Token.Comma
    found: (Token.Ident "b")
    found: Token.Colon
    found: (Token.Ident "i32")
    found: Token.Close_paren
    found: Token.Skinny_arrow
    found: (Token.Ident "i32")
    found: Token.Eql
    found: Token.Open_brack
    found: (Token.Ident "a")
    found: Token.Star
    found: (Token.Ident "b")
    found: Token.Close_brack
    found: Token.Let
    found: (Token.Ident "my_complex_fn")
    found: Token.Double_colon
    found: Token.Open_paren
    found: (Token.Ident "a")
    found: Token.Colon
    found: (Token.Ident "i32")
    found: Token.Comma
    found: (Token.Ident "b")
    found: Token.Colon
    found: (Token.Ident "i32")
    found: Token.Comma
    found: (Token.Ident "c")
    found: Token.Colon
    found: (Token.Ident "i32")
    found: Token.Close_paren
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
    found: Token.Dash
    found: (Token.Ident "res2")
    found: Token.Semi_colon
    found: (Token.Ident "res3")
    found: Token.Close_brack
    found: Token.Let
    found: (Token.Ident "main")
    found: Token.Double_colon
    found: Token.Open_paren
    found: Token.Close_paren
    found: Token.Eql
    found: Token.Open_brack
    found: (Token.Ident "std_c")
    found: Token.Dot
    found: (Token.Ident "printf")
    found: Token.Open_paren
    found: (Token.String "add(6, 7) = %d\n")
    found: Token.Comma
    found: (Token.Ident "add")
    found: Token.Open_paren
    found: (Token.Number 6)
    found: Token.Comma
    found: (Token.Number 7)
    found: Token.Close_paren
    found: Token.Close_paren
    found: Token.Semi_colon
    found: (Token.Ident "std_c")
    found: Token.Dot
    found: (Token.Ident "printf")
    found: Token.Open_paren
    found: (Token.String "mul(1, 1) = %d\n")
    found: Token.Comma
    found: (Token.Ident "mul")
    found: Token.Open_paren
    found: (Token.Number 1)
    found: Token.Comma
    found: (Token.Number 1)
    found: Token.Close_paren
    found: Token.Close_paren
    found: Token.Semi_colon
    found: (Token.Ident "std_c")
    found: Token.Dot
    found: (Token.Ident "printf")
    found: Token.Open_paren
    found: (Token.String "my_complex_fn(2, 1, 5) = %d\n")
    found: Token.Comma
    found: (Token.Ident "my_complex_fn")
    found: Token.Open_paren
    found: (Token.Number 2)
    found: Token.Comma
    found: (Token.Number 1)
    found: Token.Comma
    found: (Token.Number 5)
    found: Token.Close_paren
    found: Token.Close_paren
    found: Token.Semi_colon
    found: (Token.Number 0)
    found: Token.Close_brack
    found: Token.Eof
    File: complex_types
    found: Token.Hash
    found: Token.Open_square
    found: Token.Derive
    found: Token.Open_paren
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
    found: (Token.Ident "show")
    found: Token.Double_colon
    found: (Token.Ident "self")
    found: Token.Eql
    found: Token.Match
    found: (Token.Ident "self")
    found: Token.With
    found: Token.Bar
    found: Token.Dot
    found: (Token.Ident "programmer")
    found: Token.Skinny_arrow
    found: (Token.String "programmer")
    found: Token.Bar
    found: Token.Dot
    found: (Token.Ident "other")
    found: (Token.Ident "s")
    found: Token.Skinny_arrow
    found: (Token.Ident "s")
    found: Token.Bar
    found: Token.Dot
    found: (Token.Ident "sales_rep")
    found: Token.Skinny_arrow
    found: (Token.String "sales rep")
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
    found: (Token.String "Hello, my name is %s and I am a %s")
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
    found: Token.Let
    found: (Token.Ident "main")
    found: Token.Double_colon
    found: Token.Open_paren
    found: Token.Close_paren
    found: Token.Eql
    found: Token.Open_brack
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
    found: (Token.Ident "based_dev")
    found: Token.Dot
    found: (Token.Ident "introduce")
    found: Token.Open_paren
    found: Token.Close_paren
    found: Token.Semi_colon
    found: (Token.Ident "println")
    found: Token.Bang
    found: Token.Open_paren
    found: (Token.String "Hourly rate: %d")
    found: Token.Comma
    found: (Token.Ident "based_dev")
    found: Token.Dot
    found: (Token.Ident "job")
    found: Token.Dot
    found: (Token.Ident "hourly")
    found: Token.Open_paren
    found: (Token.Number 40)
    found: Token.Close_paren
    found: Token.Close_paren
    found: Token.Semi_colon
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
    found: (Token.Ident "prob_some_creep")
    found: Token.Dot
    found: (Token.Ident "introduce")
    found: Token.Open_paren
    found: Token.Close_paren
    found: Token.Semi_colon
    found: Token.Close_brack
    found: Token.Eof
    File: currying_functions
    found: Token.Let
    found: (Token.Ident "mul")
    found: Token.Double_colon
    found: Token.Open_paren
    found: (Token.Ident "a")
    found: Token.Colon
    found: (Token.Ident "i32")
    found: Token.Comma
    found: (Token.Ident "b")
    found: Token.Colon
    found: (Token.Ident "i32")
    found: Token.Close_paren
    found: Token.Skinny_arrow
    found: (Token.Ident "i32")
    found: Token.Eql
    found: Token.Open_brack
    found: (Token.Ident "a")
    found: Token.Star
    found: (Token.Ident "b")
    found: Token.Close_brack
    found: Token.Let
    found: (Token.Ident "add")
    found: Token.Double_colon
    found: Token.Open_paren
    found: (Token.Ident "a")
    found: Token.Colon
    found: (Token.Ident "i32")
    found: Token.Comma
    found: (Token.Ident "b")
    found: Token.Colon
    found: (Token.Ident "i32")
    found: Token.Close_paren
    found: Token.Skinny_arrow
    found: (Token.Ident "i32")
    found: Token.Eql
    found: Token.Open_brack
    found: (Token.Ident "a")
    found: Token.Plus
    found: (Token.Ident "b")
    found: Token.Close_brack
    found: Token.Let
    found: (Token.Ident "main")
    found: Token.Double_colon
    found: Token.Open_paren
    found: Token.Close_paren
    found: Token.Eql
    found: Token.Open_brack
    found: Token.Let
    found: (Token.Ident "result")
    found: Token.Eql
    found: (Token.Ident "mul")
    found: Token.Open_paren
    found: (Token.Number 5)
    found: Token.Comma
    found: (Token.Number 3)
    found: Token.Close_paren
    found: Token.Semi_colon
    found: (Token.Ident "std_c")
    found: Token.Dot
    found: (Token.Ident "printf")
    found: Token.Open_paren
    found: (Token.String "5 * 3 = %d\n")
    found: Token.Comma
    found: (Token.Ident "result")
    found: Token.Close_paren
    found: Token.Semi_colon
    found: (Token.Ident "std_c")
    found: Token.Dot
    found: (Token.Ident "printf")
    found: Token.Open_paren
    found: (Token.String "2 + 3 = %d\n")
    found: Token.Comma
    found: (Token.Ident "add")
    found: Token.Open_paren
    found: (Token.Number 2)
    found: Token.Comma
    found: (Token.Number 3)
    found: Token.Close_paren
    found: Token.Close_paren
    found: Token.Semi_colon
    found: (Token.Number 0)
    found: Token.Close_brack
    found: Token.Eof
    File: ffi_example
    found: Token.Let
    found: (Token.Ident "main")
    found: Token.Double_colon
    found: Token.Open_paren
    found: Token.Close_paren
    found: Token.Eql
    found: Token.Open_brack
    found: (Token.Ident "std_c")
    found: Token.Dot
    found: (Token.Ident "printf")
    found: Token.Open_paren
    found: (Token.String "Hello world! %d\n")
    found: Token.Comma
    found: (Token.Number 32)
    found: Token.Close_paren
    found: Token.Semi_colon
    found: (Token.Number 0)
    found: Token.Close_brack
    found: Token.Eof
    File: module_test
    found: Token.Module
    found: (Token.Ident "Math")
    found: Token.Open_brack
    found: Token.Export
    found: (Token.Ident "add")
    found: Token.Export
    found: Token.Open_brack
    found: (Token.Ident "sub")
    found: Token.As
    found: (Token.Ident "subtract")
    found: Token.Close_brack
    found: Token.Let
    found: (Token.Ident "add")
    found: Token.Double_colon
    found: Token.Open_paren
    found: (Token.Ident "a")
    found: Token.Colon
    found: (Token.Ident "i32")
    found: Token.Comma
    found: (Token.Ident "b")
    found: Token.Colon
    found: (Token.Ident "i32")
    found: Token.Close_paren
    found: Token.Skinny_arrow
    found: (Token.Ident "i32")
    found: Token.Eql
    found: Token.Open_brack
    found: (Token.Ident "a")
    found: Token.Plus
    found: (Token.Ident "b")
    found: Token.Close_brack
    found: Token.Let
    found: (Token.Ident "sub")
    found: Token.Double_colon
    found: Token.Open_paren
    found: (Token.Ident "a")
    found: Token.Colon
    found: (Token.Ident "i32")
    found: Token.Comma
    found: (Token.Ident "b")
    found: Token.Colon
    found: (Token.Ident "i32")
    found: Token.Close_paren
    found: Token.Skinny_arrow
    found: (Token.Ident "i32")
    found: Token.Eql
    found: Token.Open_brack
    found: (Token.Ident "a")
    found: Token.Dash
    found: (Token.Ident "b")
    found: Token.Close_brack
    found: Token.Close_brack
    found: Token.Let
    found: (Token.Ident "main")
    found: Token.Double_colon
    found: Token.Open_paren
    found: Token.Close_paren
    found: Token.Eql
    found: Token.Open_brack
    found: Token.Let
    found: (Token.Ident "x")
    found: Token.Eql
    found: (Token.Ident "Math")
    found: Token.Dot
    found: (Token.Ident "add")
    found: Token.Open_paren
    found: (Token.Number 10)
    found: Token.Comma
    found: (Token.Number 5)
    found: Token.Close_paren
    found: Token.Semi_colon
    found: Token.Let
    found: (Token.Ident "y")
    found: Token.Eql
    found: (Token.Ident "Math")
    found: Token.Dot
    found: (Token.Ident "sub")
    found: Token.Open_paren
    found: (Token.Number 10)
    found: Token.Comma
    found: (Token.Number 5)
    found: Token.Close_paren
    found: Token.Semi_colon
    found: (Token.Ident "std_c")
    found: Token.Dot
    found: (Token.Ident "printf")
    found: Token.Open_paren
    found: (Token.String "%d\n")
    found: Token.Comma
    found: (Token.Ident "x")
    found: Token.Close_paren
    found: Token.Semi_colon
    found: (Token.Ident "std_c")
    found: Token.Dot
    found: (Token.Ident "printf")
    found: Token.Open_paren
    found: (Token.String "%d\n")
    found: Token.Comma
    found: (Token.Ident "y")
    found: Token.Close_paren
    found: Token.Semi_colon
    found: (Token.Ident "x")
    found: Token.Plus
    found: (Token.Ident "y")
    found: Token.Close_brack
    found: Token.Eof
    File: open_statements
    found: Token.Open
    found: (Token.Ident "std")
    found: Token.Let
    found: (Token.Ident "my_func")
    found: Token.Double_colon
    found: Token.Open_paren
    found: Token.Close_paren
    found: Token.Skinny_arrow
    found: (Token.Ident "i32")
    found: Token.Eql
    found: Token.Open_brack
    found: (Token.Number 123)
    found: Token.Close_brack
    found: Token.Let
    found: (Token.Ident "main")
    found: Token.Double_colon
    found: Token.Open_paren
    found: Token.Close_paren
    found: Token.Eql
    found: Token.Open_brack
    found: Token.Let
    found: (Token.Ident "result")
    found: Token.Eql
    found: (Token.Ident "my_func")
    found: Token.Open_paren
    found: Token.Close_paren
    found: Token.Semi_colon
    found: (Token.Ident "std_c")
    found: Token.Dot
    found: (Token.Ident "printf")
    found: Token.Open_paren
    found: (Token.String "Result: %d\n")
    found: Token.Comma
    found: (Token.Ident "result")
    found: Token.Close_paren
    found: Token.Semi_colon
    found: (Token.Number 0)
    found: Token.Close_brack
    found: Token.Eof
    File: stdlib_test
    found: Token.Let
    found: (Token.Ident "main")
    found: Token.Double_colon
    found: Token.Open_paren
    found: Token.Close_paren
    found: Token.Eql
    found: Token.Open_brack
    found: Token.Let
    found: (Token.Ident "result")
    found: Token.Eql
    found: (Token.Ident "std_math")
    found: Token.Dot
    found: (Token.Ident "square")
    found: Token.Open_paren
    found: (Token.Number 5)
    found: Token.Close_paren
    found: Token.Semi_colon
    found: (Token.Ident "std_c")
    found: Token.Dot
    found: (Token.Ident "printf")
    found: Token.Open_paren
    found: (Token.String "Square of 5: %d\n")
    found: Token.Comma
    found: (Token.Ident "result")
    found: Token.Close_paren
    found: Token.Semi_colon
    found: (Token.Ident "result")
    found: Token.Close_brack
    found: Token.Eof
    File: tags_and_macros
    found: Token.Hash
    found: Token.Open_square
    found: (Token.Ident "no_mangle")
    found: Token.Close_square
    found: Token.Let
    found: (Token.Ident "my_func")
    found: Token.Double_colon
    found: Token.Open_paren
    found: Token.Close_paren
    found: Token.Skinny_arrow
    found: (Token.Ident "i32")
    found: Token.Eql
    found: Token.Open_brack
    found: (Token.Number 42)
    found: Token.Close_brack
    found: Token.Let
    found: (Token.Ident "main")
    found: Token.Double_colon
    found: Token.Open_paren
    found: Token.Close_paren
    found: Token.Eql
    found: Token.Open_brack
    found: (Token.Ident "std_c")
    found: Token.Dot
    found: (Token.Ident "printf")
    found: Token.Open_paren
    found: (Token.String "Tagged function result: %d\n")
    found: Token.Comma
    found: (Token.Ident "my_func")
    found: Token.Open_paren
    found: Token.Close_paren
    found: Token.Close_paren
    found: Token.Semi_colon
    found: (Token.Number 0)
    found: Token.Close_brack
    found: Token.Eof
    |}]
;;
