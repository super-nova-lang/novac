let rec print_diff ~ppx xs =
  match xs with
  | x :: xt ->
    Format.printf "found: %s\n" (ppx x);
    print_diff ~ppx xt
  | [] -> ()
;;

let test f t =
  let tokens = Lexer.lex f t in
  let nodes = Parser.parse tokens in
  print_diff ~ppx:Node.show nodes
;;

let%expect_test "open-statements" =
  test
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
  |};
  [%expect
    {|
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

let%expect_test "basic-functions" =
  test
    "basic-functions"
    {|
    let add :: a: i32, b: i32 = a + b
    let sub :: a, b -> i32 = a - b
    let mul :: a, b -> i32 = a * b
  |};
  [%expect]
;;

let%expect_test "currying-functions" =
  test
    "currying-functions"
    {|
    let mul_5 :: mul <- 5
    let just_15 :: mul <- 5, 3
  |};
  [%expect]
;;

let%expect_test "tags-and-macros" =
  test
    "tags-and-macros"
    {|
    #[recusive]
    #[derive(Show, PrettyPrint)]
    let my_type :: () = "ahhh"
  |};
  [%expect]
;;

let%expect_test "enums" =
  test
    "enums"
    {|
    #[derive(Show, PrettyPrint)]
    let Job ::
      salary,
      ?language = "c++", (* #eww *)
    = enum {
      programmer -> struct {
        language : string,
      },
      other -> string,
      sales_rep,
    } with {
      salary : i32

      let hourly :: self, hours = self.salary / hours
    }
  |};
  [%expect]
;;

let%expect_test "structs" =
  test
    "structs"
    {|
    #[derive(Show, PrettyPrint)]
    let Person ::
      name, age, job
    = struct {
      name : string = name,
      age  : u8     = age,
      job  : job    = job,
    } with {
      let introduce :: self =
        println!("Hello, my name is {} and I am a {}", self.name, self.job.show())
    }

    let based_dev = Person(
      "Ashton", 19,
      Job.programmer(1_000, "nova")
    )

    let prob_some_creep = Person(
      "Joe Shmoe", 37,
      .sales_rep(),
    )
  |};
  [%expect]
;;

let%expect_test "macros-and-derive" =
  test
    "macros-and-derive"
    {|
    let my_printf ::
      fmt, args...
    = macro {
      (* Macro stuff here... *)
    }

    my_printf!("Hello world!")
    my_printf!("This is {}", "nova")

    let my_derive :: tt
    = derive {
      (* Macro stuff here... *)
    }

    #[derive(my_derive)]
    let some_struct :: () = struct {}

    let my_derive_with_params :: tt, a, b
    = derive {
      (* Macro stuff here... *)
    }

    #[derive(my_derive_with_params("a", "b"))]
    let some_struct :: () = struct {}
  |};
  [%expect]
;;
