let rec print_diff ~ppx xs =
  match xs with
  | x :: xt ->
    Format.printf "found: %s\n" (ppx x);
    print_diff ~ppx xt
  | [] -> ()
;;

let%expect_test "parse open statements" =
  let tokens =
    Lexer.lex
      "open-statements.ml"
      {|
    open MyModule
    open MyModule.SomeMod
    open Panic with { panic }
    open Assert with {
      assert,
      Static.assert_static,
      assert_eq as is_eq,
    }
  |}
  in
  let nodes = Parser.parse tokens in
  print_diff ~ppx:Node.show nodes;
  [%expect {|
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
    |}]
;;
