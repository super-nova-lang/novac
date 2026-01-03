open Token
open Utils

let lex file input =
  let rec lex_doc_comment cs row col acc =
    match cs with
    | [] -> String.concat "" (List.rev acc), [], row, col
    | '*' :: ')' :: rest -> String.concat "" (List.rev acc), rest, row, col + 2
    | '\n' :: rest -> lex_doc_comment rest (row + 1) 1 ("\n" :: acc)
    | c :: rest -> lex_doc_comment rest row (col + 1) (String.make 1 c :: acc)
  in
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
    | '(' :: '*' :: '!' :: cs ->
      let loc = { file; row; col } in
      let content, rest, row', col' = lex_doc_comment cs row (col + 3) [] in
      (Doc_comment content, loc) :: loop rest row' col'
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
    | '\'' :: '\\' :: c :: '\'' :: cs ->
      let mapped = map_escape c in
      (Char mapped, loc) :: loop cs row (col + 4)
    | '\'' :: c :: '\'' :: cs -> (Char c, loc) :: loop cs row (col + 3)
    | '\'' :: c :: cs when is_alpha c || c = '_' ->
      let ident, rest = consume ~f:is_ident (String.make 1 c) cs in
      let full = "'" ^ ident in
      (Token.from_string full, loc) :: loop rest row (col + String.length full)
    | '\'' :: _ -> raise_error ~phase:Lexer "Invalid char or type variable literal"
    (* Read Comment *)
    (* | '(' :: '*' :: cs -> loop (consume_comment 1 cs) *)
    (* Read ident *)
    | c :: cs when is_alpha c ->
      let s, rest = consume ~f:is_ident (String.make 1 c) cs in
      (Token.from_string s, loc) :: loop rest row (col + String.length s)
    (* Read number *)
    | c :: cs when is_numer c ->
      let s, rest = consume ~f:(fun c -> is_numer c || c = '_') (String.make 1 c) cs in
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
    | ':' :: '=' :: cs -> (Walrus, loc) :: loop cs row (col + 2)
    | '+' :: '+' :: cs -> (Concat, loc) :: loop cs row (col + 2)
    | '+' :: '=' :: cs -> (Plus_eql, loc) :: loop cs row (col + 2)
    | '-' :: '=' :: cs -> (Minus_eql, loc) :: loop cs row (col + 2)
    | '*' :: '=' :: cs -> (Star_eql, loc) :: loop cs row (col + 2)
    | '/' :: '=' :: cs -> (Slash_eql, loc) :: loop cs row (col + 2)
    | '.' :: '.' :: '.' :: cs -> (Ellipsis, loc) :: loop cs row (col + 3)
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
