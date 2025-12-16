type t =
  (* Keywords *)
  | Let
  | Open
  | Match
  | With
  | Return
  | If
  | Else
  | Struct
  | Mut
  | Defer
  (* Tokens *)
  (* - Double *)
  | Double_colon (* :: *)
  | Fat_arrow (* => *)
  | Double_eql
  | Skinny_arrow (* -> *)
  | Back_arrow (* <- *)
  | Pipe (* |> *)
  | Or (* || *)
  | Bitwise_or (* |. *)
  | And (* && *)
  | Bitwise_and (* &. *)
  | Lesser_eql
  | Greater_eql
  | UnitToken (* () *)
  (* - Single*)
  | Tilde
  | Back_tick
  | Bang
  | At
  | Hash
  | Dollar
  | Mod
  | Carrot
  | Amper
  | Star
  | Open_paren
  | Close_paren
  | Open_brack
  | Close_brack
  | Open_square
  | Close_square
  | Low_dash
  | Dash
  | Plus
  | Eql
  | Bar
  | Back_slash
  | Colon
  | Semi_colon
  | Lesser
  | Comma
  | Greater
  | Dot
  | Question
  | Forward_slash
  (* Literals *)
  | String of string
  | Char of char
  | Ident of string
  | Number of int
  | Eof
  | Unknown of char
[@@deriving show]

let from_string = function
  | "let" -> Let
  | "open" -> Open
  | "match" -> Match
  | "with" -> With
  | "return" -> Return
  | "if" -> If
  | "else" -> Else
  | "struct" -> Struct
  | "mut" -> Mut
  | "defer" -> Defer
  | s -> Ident s
;;
