open Token
open Utils

let lex file input =
  let rec lex_comment cs row col =
    let loc = { file; row; col } in
    match cs with
    | [] -> [ Eof, loc ]
    | '*' :: ')' :: cs -> loop cs row col
    | _ :: cs -> lex_comment cs row (col + 1)
  and loop cs row col =
    let loc = { file; row; col } in
    match cs with
    | [] -> [ Eof, loc ]
    | '(' :: '*' :: cs -> lex_comment cs row (col + 2)
    (* Newline *)
    | '\n' :: cs -> loop cs (row + 1) 1
    (* Skip whitespace*)
    | c :: cs when is_white c -> loop cs row (col + 1)
    (* Consume string *)
    | '"' :: cs ->
      let s, rest = consume_string "" cs in
      let new_col = col + String.length s + 2 in
      (String s, loc) :: loop rest row new_col
    (* Consume char *)
    | '\'' :: cs ->
      let c, rest = consume_char cs in
      (Char c, loc) :: loop rest row (col + 3)
    (* Read Comment *)
    (* | '(' :: '*' :: cs -> loop (consume_comment 1 cs) *)
    (* Read ident *)
    | c :: cs when is_alpha c ->
      let s, rest = consume ~f:is_ident (String.make 1 c) cs in
      (Token.from_string s, loc) :: loop rest row (col + String.length s)
    (* Read number *)
    | c :: cs when is_numer c ->
      let s, rest = consume ~f:is_numer (String.make 1 c) cs in
      (Number (int_of_string s), loc) :: loop rest row (col + String.length s)
    (* Tokens *)
    (* - Double *)
    | ':' :: ':' :: cs -> (Double_colon, loc) :: loop cs row (col + 2)
    | '=' :: '>' :: cs -> (Fat_arrow, loc) :: loop cs row (col + 2)
    | '=' :: '=' :: cs -> (Double_eql, loc) :: loop cs row (col + 2)
    | '-' :: '>' :: cs -> (Skinny_arrow, loc) :: loop cs row (col + 2)
    | '|' :: '>' :: cs -> (Pipe, loc) :: loop cs row (col + 2)
    | '|' :: '|' :: cs -> (Or, loc) :: loop cs row (col + 2)
    | '|' :: '.' :: cs -> (Bitwise_or, loc) :: loop cs row (col + 2)
    | '&' :: '&' :: cs -> (And, loc) :: loop cs row (col + 2)
    | '&' :: '.' :: cs -> (Bitwise_and, loc) :: loop cs row (col + 2)
    | '<' :: '=' :: cs -> (Lesser_eql, loc) :: loop cs row (col + 2)
    | '<' :: '-' :: cs -> (Back_arrow, loc) :: loop cs row (col + 2)
    | '>' :: '=' :: cs -> (Greater_eql, loc) :: loop cs row (col + 2)
    (* - Single *)
    | '~' :: cs -> (Tilde, loc) :: loop cs row (col + 1)
    | '`' :: cs -> (Back_tick, loc) :: loop cs row (col + 1)
    | '!' :: cs -> (Bang, loc) :: loop cs row (col + 1)
    | '@' :: cs -> (At, loc) :: loop cs row (col + 1)
    | '#' :: cs -> (Hash, loc) :: loop cs row (col + 1)
    | '$' :: cs -> (Dollar, loc) :: loop cs row (col + 1)
    | '%' :: cs -> (Mod, loc) :: loop cs row (col + 1)
    | '^' :: cs -> (Carrot, loc) :: loop cs row (col + 1)
    | '&' :: cs -> (Amper, loc) :: loop cs row (col + 1)
    | '*' :: cs -> (Star, loc) :: loop cs row (col + 1)
    | '(' :: cs -> (Open_paren, loc) :: loop cs row (col + 1)
    | ')' :: cs -> (Close_paren, loc) :: loop cs row (col + 1)
    | '{' :: cs -> (Open_brack, loc) :: loop cs row (col + 1)
    | '}' :: cs -> (Close_brack, loc) :: loop cs row (col + 1)
    | '[' :: cs -> (Open_square, loc) :: loop cs row (col + 1)
    | ']' :: cs -> (Close_square, loc) :: loop cs row (col + 1)
    | '_' :: cs -> (Low_dash, loc) :: loop cs row (col + 1)
    | '-' :: cs -> (Dash, loc) :: loop cs row (col + 1)
    | '+' :: cs -> (Plus, loc) :: loop cs row (col + 1)
    | '=' :: cs -> (Eql, loc) :: loop cs row (col + 1)
    | '|' :: cs -> (Bar, loc) :: loop cs row (col + 1)
    | '\\' :: cs -> (Back_slash, loc) :: loop cs row (col + 1)
    | ':' :: cs -> (Colon, loc) :: loop cs row (col + 1)
    | ';' :: cs -> (Semi_colon, loc) :: loop cs row (col + 1)
    | '<' :: cs -> (Lesser, loc) :: loop cs row (col + 1)
    | '>' :: cs -> (Greater, loc) :: loop cs row (col + 1)
    | ',' :: cs -> (Comma, loc) :: loop cs row (col + 1)
    | '.' :: cs -> (Dot, loc) :: loop cs row (col + 1)
    | '?' :: cs -> (Question, loc) :: loop cs row (col + 1)
    | '/' :: cs -> (Forward_slash, loc) :: loop cs row (col + 1)
    (* Unknown *)
    | c :: _ -> (Unknown c, loc) :: []
  in
  loop (explode input) 1 1
;;

let lex_from_file file =
  let contents = read_entire_file file in
  lex file contents
;;
