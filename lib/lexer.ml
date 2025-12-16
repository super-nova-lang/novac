open Token
open Utils

let lex input =
  let rec loop cs =
    match cs with
    | [] -> [ Eof ]
    (* Skip whitespace*)
    | c :: cs when is_white c -> loop cs
    (* Consume string *)
    | '"' :: cs ->
      let s, rest = consume_string "" cs in
      String s :: loop rest
    (* Consume char *)
    | '\'' :: cs ->
      let c, rest = consume_char cs in
      Char c :: loop rest
    (* Read Comment *)
    | '(' :: '*' :: cs -> loop (consume_comment 1 cs)
    (* Read ident *)
    | c :: cs when is_alpha c ->
      let s, rest = consume ~f:is_ident (String.make 1 c) cs in
      Token.from_string s :: loop rest
    (* Read number *)
    | c :: cs when is_numer c ->
      let s, rest = consume ~f:is_numer (String.make 1 c) cs in
      Number s :: loop rest
    (* Tokens *)
    (* - Double *)
    | ':' :: ':' :: cs -> Double_colon :: loop cs
    | '=' :: '>' :: cs -> Fat_arrow :: loop cs
    | '=' :: '=' :: cs -> Double_eql :: loop cs
    | '-' :: '>' :: cs -> Skinny_arrow :: loop cs
    | '|' :: '>' :: cs -> Pipe :: loop cs
    | '|' :: '|' :: cs -> Or :: loop cs
    | '|' :: '.' :: cs -> Bitwise_or :: loop cs
    | '&' :: '&' :: cs -> And :: loop cs
    | '&' :: '.' :: cs -> Bitwise_and :: loop cs
    | '<' :: '=' :: cs -> Lesser_eql :: loop cs
    | '<' :: '-' :: cs -> Back_arrow :: loop cs
    | '>' :: '=' :: cs -> Greater_eql :: loop cs
    (* - Single *)
    | '~' :: cs -> Tilde :: loop cs
    | '`' :: cs -> Back_tick :: loop cs
    | '!' :: cs -> Bang :: loop cs
    | '@' :: cs -> At :: loop cs
    | '#' :: cs -> Hash :: loop cs
    | '$' :: cs -> Dollar :: loop cs
    | '%' :: cs -> Mod :: loop cs
    | '^' :: cs -> Carrot :: loop cs
    | '&' :: cs -> Amper :: loop cs
    | '*' :: cs -> Star :: loop cs
    | '(' :: cs -> Open_paren :: loop cs
    | ')' :: cs -> Close_paren :: loop cs
    | '{' :: cs -> Open_brack :: loop cs
    | '}' :: cs -> Close_brack :: loop cs
    | '[' :: cs -> Open_square :: loop cs
    | ']' :: cs -> Close_square :: loop cs
    | '_' :: cs -> Low_dash :: loop cs
    | '-' :: cs -> Dash :: loop cs
    | '+' :: cs -> Plus :: loop cs
    | '=' :: cs -> Eql :: loop cs
    | '|' :: cs -> Bar :: loop cs
    | '\\' :: cs -> Back_slash :: loop cs
    | ':' :: cs -> Colon :: loop cs
    | ';' :: cs -> Semi_colon :: loop cs
    | '<' :: cs -> Lesser :: loop cs
    | '>' :: cs -> Greater :: loop cs
    | ',' :: cs -> Comma :: loop cs
    | '.' :: cs -> Dot :: loop cs
    | '?' :: cs -> Question :: loop cs
    | '/' :: cs -> Forward_slash :: loop cs
    (* Unknown *)
    | c :: _ -> Unknown c :: []
  in
  loop (explode input)
;;

let lex_from_file file =
  let contents = read_entire_file file in
  lex contents
;;
