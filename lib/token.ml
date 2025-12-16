type t =
  (* Keywords *)
  | Let
  (* Tokens *)
  (* - Double *)
  | Double_colon (* :: *)
  | Fat_arrow (* => *)
  | Double_eql
  | Skinny_arrow (* -> *)
  | Pipe (* |> *)
  | Or (* || *)
  | Bitwise_or (* |. *)
  | And (* && *)
  | Bitwise_and (* &. *)
  | Lesser_eql
  | Greater_eql
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
  | Number of string
  | Eof
  | Unknown of char
[@@deriving show]
