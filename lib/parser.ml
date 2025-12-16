open Token
open Node

(* Helpers *)
let rec parse_type toks =
  match toks with
  | Ident t :: rest -> Some (Type_ident t, rest)
  | _ -> None

and parse_params toks acc =
  match toks with
  | Skinny_arrow :: _ | Eql :: _ -> List.rev acc, toks
  | Ident name :: Colon :: rest ->
    (match parse_type rest with
     | Some (t, Comma :: r) -> parse_params r ((name, Some t) :: acc)
     | Some (t, r) -> parse_params r ((name, Some t) :: acc)
     | None -> List.rev acc, rest)
  | Ident name :: Comma :: rest -> parse_params rest ((name, None) :: acc)
  | Ident name :: rest -> parse_params rest ((name, None) :: acc)
  | _ :: rest -> parse_params rest acc
  | [] -> List.rev acc, []

and consume_expr current toks =
  match toks with
  | Semi_colon :: rest -> Expr (List.rev current), rest, true (* delimited *)
  | Close_brack :: _ -> Expr (List.rev current), toks, false (* end of block *)
  | [] -> Expr (List.rev current), [], false
  | t :: rest -> consume_expr (t :: current) rest

and parse_block toks acc =
  match toks with
  | Close_brack :: rest -> List.rev acc, rest
  | _ ->
    let node, rest, delimited = consume_expr [] toks in
    (match rest, delimited with
     | Close_brack :: final, _ -> List.rev (node :: acc), final
     | _, true -> parse_block rest (node :: acc) (* Semi found, continue *)
     | _, false -> List.rev (node :: acc), rest)

and parse_curry_arg toks =
  match toks with
  | Ident k :: Eql :: rest ->
    let expr, final, _ = consume_expr [] rest in
    (match expr with
     | Expr _ as ex -> Named (k, [ ex ]), final
     | _ -> Named (k, []), final)
  | _ ->
    let expr, final, _ = consume_expr [] toks in
    (match expr with
     | Expr _ as ex -> Positional [ ex ], final
     | _ -> Positional [], final)

and parse_curry_args toks acc =
  match toks with
  | [] -> List.rev acc, []
  (* Handle delimiters that might end the statement *)
  | Semi_colon :: rest -> List.rev acc, rest
  | Comma :: rest ->
    let arg, next = parse_curry_arg rest in
    parse_curry_args next (arg :: acc)
  | _ ->
    (* Try parsing the first/next argument if no comma seen yet (start of list) *)
    let arg, next = parse_curry_arg toks in
    (match next with
     | Comma :: r -> parse_curry_args r (arg :: acc)
     | _ -> List.rev (arg :: acc), next)

and parse_curried name toks =
  match toks with
  | Ident target :: Back_arrow :: rest ->
    let args, final = parse_curry_args rest [] in
    (* Consume trailing semicolon if present *)
    let final' =
      match final with
      | Semi_colon :: f -> f
      | _ -> final
    in
    Curry_Decl { name; target_fn = target; args } :: parse' final'
  | _ -> Unhandled (Ident "Invalid Curry Syntax") :: parse' toks

and parse_decl toks =
  match toks with
  | Ident name :: Double_colon :: rest ->
    (match rest with
     (* Lookahead: If we see Ident followed by <-, it is a Curry Decl *)
     | Ident _ :: Back_arrow :: _ -> parse_curried name rest
     | _ ->
       (* Existing Standard Decl Logic *)
       let params, rest_after_params = parse_params rest [] in
       let ret_type, rest_after_type =
         match rest_after_params with
         | Skinny_arrow :: t_toks ->
           (match parse_type t_toks with
            | Some (t, r) -> Some t, r
            | None -> None, t_toks)
         | _ -> None, rest_after_params
       in
       (match rest_after_type with
        | Eql :: Open_brack :: body_toks ->
          let body, final = parse_block body_toks [] in
          Decl { name; params; ret_type; body } :: parse' final
        | Eql :: body_toks ->
          let expr, final, _ = consume_expr [] body_toks in
          Decl { name; params; ret_type; body = [ expr ] } :: parse' final
        | _ -> Unhandled (Ident "Expected Body") :: parse' rest_after_type))
  | _ -> Unhandled (Ident "Expected Decl Pattern") :: parse' toks

and parse_tag toks =
  match toks with
  | Open_square :: Ident t :: Close_square :: rest -> Tag t :: parse' rest
  | _ -> Unhandled (Ident "Expected Tag Pattern") :: parse' toks

and parse' toks =
  match toks with
  | [] -> []
  | [ Eof ] -> []
  | Open :: Ident mod_name :: rest -> Open_mod mod_name :: parse' rest
  | Hash :: rest -> parse_tag rest
  | Let :: rest -> parse_decl rest
  | t :: tail -> Unhandled t :: parse' tail
;;

let parse toks = parse' toks
