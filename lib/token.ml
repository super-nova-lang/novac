type t =
  (* Keywords *)
  | Let
  | Open
  | Import
  | Module
  | Export
  | Match
  | With
  | As
  | Return
  | If
  | Else
  | Struct
  | Enum
  | Macro
  | Derive
  | Mut
  | Defer
  | True
  | False
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
  | Ellipsis (* ... *)
  | Walrus (* := *)
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

type loc =
  { file : string
  ; row : int
  ; col : int
  }

let show_loc { file; row; col } = Printf.sprintf "%s:%d:%d" file row col

let from_string = function
  | "let" -> Let
  | "open" -> Open
  | "import" -> Import
  | "module" -> Module
  | "export" -> Export
  | "match" -> Match
  | "as" -> As
  | "with" -> With
  | "return" -> Return
  | "if" -> If
  | "else" -> Else
  | "struct" -> Struct
  | "enum" -> Enum
  | "macro" -> Macro
  | "derive" -> Derive
  | "mut" -> Mut
  | "defer" -> Defer
  | "true" -> True
  | "false" -> False
  | s -> Ident s
;;
