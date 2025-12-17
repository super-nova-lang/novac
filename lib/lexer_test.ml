open Token

let reset_ppf = Spectrum.prepare_ppf Format.std_formatter

let rec print_diff ppx xs ys =
  match xs, ys with
  | x :: xt, y :: yt when x = y -> print_diff ppx xt yt
  | x :: xt, y :: yt ->
    Format.printf
      "@{<red>Expected@}: @{<bold>%s@}, @{<green>Got@}: @{<bold>%s@}\n"
      (ppx x)
      (ppx y);
    print_diff ppx xt yt
  | x :: xt, [] ->
    Format.printf
      "@{<red>Expected@}: @{<bold>%s@}, @{<green>Got@}: @{<bold>None@}\n"
      (ppx x);
    print_diff ppx xt []
  | [], y :: yt ->
    Format.printf
      "@{<red>Expected@}: @{<bold>None@}, @{<green>Got@}: @{<bold>%s@}\n"
      (ppx y);
    print_diff ppx [] yt
  | [], [] -> ()
;;

let test ~expected ~got =
  if expected = got
  then true
  else (
    print_diff Token.show expected got;
    reset_ppf ();
    false)
;;

let spec_tokens =
  (* open Testing *)
  [ Open
  ; Ident "Testing" (* open Panic *)
  ; Open
  ; Ident "Panic"
  ; Let (* let add :: a: i32, b: i32 -> i32 = {a + b} *)
  ; Ident "add"
  ; Double_colon
  ; Ident "a"
  ; Colon
  ; Ident "i32"
  ; Comma
  ; Ident "b"
  ; Colon
  ; Ident "i32"
  ; Skinny_arrow
  ; Ident "i32"
  ; Eql
  ; Open_brack
  ; Ident "a"
  ; Plus
  ; Ident "b"
  ; Close_brack
  ; Let (* let sub :: a, b = return a - b;*)
  ; Ident "sub"
  ; Double_colon
  ; Ident "a"
  ; Comma
  ; Ident "b"
  ; Eql
  ; Return
  ; Ident "a"
  ; Dash
  ; Ident "b"
  ; Semi_colon
  ; Let (* let mul :: a, b = { a * b } *)
  ; Ident "mul"
  ; Double_colon
  ; Ident "a"
  ; Comma
  ; Ident "b"
  ; Eql
  ; Open_brack
  ; Ident "a"
  ; Star
  ; Ident "b"
  ; Close_brack
  ; Let (* let mul_5 :: mul <- 5; *)
  ; Ident "mul_5"
  ; Double_colon
  ; Ident "mul"
  ; Back_arrow
  ; Number 5
  ; Semi_colon
  ; Hash (* #[recursive] *)
  ; Open_square
  ; Ident "recursive"
  ; Close_square
  ; Let (* let factorial :: of = { *)
  ; Ident "factorial"
  ; Double_colon
  ; Ident "of"
  ; Eql
  ; Open_brack
  ; Match (* match of with *)
  ; Ident "of"
  ; With
  ; Bar (* | 0 -> 0*)
  ; Number 0
  ; Skinny_arrow
  ; Number 1
  ; Bar (* | c if x < 0 -> panic!("...", x) *)
  ; Ident "x"
  ; If
  ; Ident "x"
  ; Lesser
  ; Number 0
  ; Skinny_arrow
  ; Ident "panic"
  ; Bang
  ; Open_paren
  ; String "factroial is not defined for negative numbers: got: {}"
  ; Comma
  ; Ident "x"
  ; Close_paren
  ; Bar (* | x -> x * factorial(x - 1) *)
  ; Ident "x"
  ; Skinny_arrow
  ; Ident "x"
  ; Star
  ; Ident "factorial"
  ; Open_paren
  ; Ident "x"
  ; Dash
  ; Number 1
  ; Close_paren
  ; Close_brack (* } *)
  ; Let (* let main :: () = { *)
  ; Ident "main"
  ; Double_colon
  ; Open_paren
  ; Close_paren
  ; Eql
  ; Open_brack
  ; Let
  ; Ident "res"
  ; Eql
  ; Ident "add"
  ; Open_paren
  ; Number 1
  ; Comma
  ; Number 5
  ; Close_paren
  ; Semi_colon
  ; Ident "assert"
  ; Bang
  ; Open_paren
  ; Ident "res"
  ; Double_eql
  ; Number 6
  ; Close_paren
  ; Semi_colon
  ; Let
  ; Ident "res"
  ; Eql
  ; Ident "sub"
  ; Open_paren
  ; Number 5
  ; Comma
  ; Number 15
  ; Close_paren
  ; Semi_colon
  ; Ident "assert"
  ; Bang
  ; Open_paren
  ; Ident "res"
  ; Double_eql
  ; Dash
  ; Number 10
  ; Close_paren
  ; Semi_colon
  ; Let
  ; Ident "res"
  ; Eql
  ; Ident "mul"
  ; Open_paren
  ; Number 2
  ; Comma
  ; Number 5
  ; Close_paren
  ; Semi_colon
  ; Ident "assert"
  ; Bang
  ; Open_paren
  ; Ident "res"
  ; Double_eql
  ; Number 10
  ; Close_paren
  ; Semi_colon
  ; Let
  ; Ident "res_curry"
  ; Eql
  ; Ident "mul_5"
  ; Open_paren
  ; Number 10
  ; Close_paren
  ; Semi_colon
  ; Ident "assert"
  ; Bang
  ; Open_paren
  ; Ident "res_curry"
  ; Double_eql
  ; Number 10
  ; Star
  ; Number 5
  ; Close_paren
  ; Semi_colon
  ; Close_brack
  ; Eof
  ]
;;

let%test "test spec" =
  let tokens = Lexer.lex Spec.content in
  test ~got:tokens ~expected:spec_tokens
;;
