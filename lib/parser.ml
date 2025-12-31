type t =
  { tokens : (Token.t * Token.loc) list
  ; mutable pos : int
  }

let create tokens = { tokens; pos = 0 }
let peek p = fst (List.nth p.tokens p.pos)
let loc p = snd (List.nth p.tokens p.pos)
let advance p = p.pos <- p.pos + 1
let is_at_end p = p.pos >= List.length p.tokens || peek p = Token.Eof

let expect p token =
  if peek p = token
  then (
    advance p;
    Ok ())
  else
    Error
      (Printf.sprintf
         "Expected %s, got %s at %s"
         (Token.show token)
         (Token.show (peek p))
         (Token.show_loc (loc p)))
;;

let consume p =
  let token = peek p in
  advance p;
  token
;;

let get_ok_or_fail = function
  | Ok v -> v
  | Error err -> failwith err
;;

let wrap_unary (u : Ast.unary_expr) : Ast.expression =
  Ast.Relational_expr (Ast.Relational_val (Ast.Additive_val (Ast.Multiplicative_val u)))
;;

let rec parse p =
  let stmts = ref [] in
  while not (is_at_end p) do
    match parse_toplevel p with
    | Ok stmt -> stmts := stmt :: !stmts
    | Error err ->
      stmts := Ast.Error err :: !stmts;
      breakout p
  done;
  List.rev !stmts

and breakout p =
  let rec aux () =
    if not (is_at_end p)
    then (
      match peek p with
      | Token.Semi_colon -> advance p
      | _ ->
        advance p;
        aux ())
  in
  aux ()

and parse_toplevel p =
  let tags = parse_tags p in
  match peek p with
  | Token.Let ->
    (match parse_decl_stmt p tags with
     | Ok stmt -> Ok (Ast.Statement stmt)
     | Error err -> Error err)
  | Token.Ident _ ->
    let current_pos = p.pos in
    advance p;
    let is_decl =
      match peek p with
      | Token.Colon | Token.Double_colon -> true
      | _ -> false
    in
    p.pos <- current_pos;
    if is_decl
    then (
      match parse_decl_stmt p tags with
      | Ok stmt -> Ok (Ast.Statement stmt)
      | Error err -> Error err)
    else (
      match parse_statement p with
      | Ok stmt -> Ok (Ast.Statement stmt)
      | Error err -> Error err)
  | _ ->
    (match parse_statement p with
     | Ok stmt -> Ok (Ast.Statement stmt)
     | Error err -> Error err)

and parse_statement p =
  match peek p with
  | Token.Open -> parse_open_stmt p
  | Token.Let -> parse_decl_stmt p []
  | Token.Return -> parse_return_stmt p
  | Token.If -> parse_if_stmt p
  | Token.Hash -> Error "Tags can only be applied to declarations"
  | _ -> parse_expression_stmt p

and parse_open_stmt p =
  advance p;
  (* consume Open *)
  let mods = parse_module_path p in
  let elements =
    if peek p = Token.With
    then (
      advance p;
      (* consume With *)
      expect p Token.Open_brack |> ignore;
      let elems = parse_list p [ Token.Close_brack ] parse_open_element in
      expect p Token.Close_brack |> ignore;
      elems)
    else []
  in
  Ok (Ast.Open_stmt { mods; elements })

and parse_module_path p =
  let rec aux acc =
    match peek p with
    | Token.Ident ident ->
      advance p;
      if peek p = Token.Dot
      then (
        advance p;
        aux (ident :: acc))
      else List.rev (ident :: acc)
    | _ -> List.rev acc
  in
  aux []

and parse_open_element p =
  let path = parse_module_path p in
  let alias =
    if peek p = Token.As
    then (
      advance p;
      (* consume As *)
      match peek p with
      | Token.Ident alias ->
        advance p;
        Some alias
      | _ -> None (* Should be an error *))
    else None
  in
  { Ast.path; alias }

and parse_tags p =
  let rec aux acc =
    if peek p = Token.Hash then aux (parse_tag p :: acc) else List.rev acc
  in
  aux []

and parse_tag p =
  expect p Token.Hash |> ignore;
  expect p Token.Open_square |> ignore;
  let tag =
    match peek p with
    | Token.Ident name ->
      advance p;
      if peek p = Token.Open_paren
      then (
        let call = parse_call_expr_with_name name p in
        Ast.Tag_call call)
      else Ast.Tag_name name
    | Token.Derive ->
      advance p;
      if peek p = Token.Open_paren
      then (
        let call = parse_call_expr_with_name "derive" p in
        Ast.Tag_call call)
      else Ast.Tag_name "derive"
    | _ -> failwith "Expected identifier in tag"
  in
  expect p Token.Close_square |> ignore;
  tag

and parse_decl_stmt p tags =
  if peek p = Token.Let then advance p;
  let name =
    match consume p with
    | Token.Ident s -> s
    | t ->
      failwith
        (Printf.sprintf
           "Expected identifier for declaration, got %s at %s"
           (Token.show t)
           (Token.show_loc (loc p)))
  in
  match peek p with
  | Token.Double_colon ->
    (* Declaration or Currying or Import *)
    advance p;
    if peek p = Token.Import
    then (
      advance p;
      expect p Token.Back_arrow |> ignore;
      let calling_conf =
        match consume p with
        | Token.String s -> s
        | t -> failwith ("Expected calling convention string, got " ^ Token.show t)
      in
      expect p Token.Comma |> ignore;
      let link_name =
        match consume p with
        | Token.String s -> s
        | t -> failwith ("Expected link name string, got " ^ Token.show t)
      in
      Ok (Ast.Decl_stmt (Ast.Import_decl { name; calling_conf; link_name })))
    else (
      let current_pos = p.pos in
      let is_curry =
        match consume p with
        | Token.Ident _ -> peek p = Token.Back_arrow
        | _ -> false
      in
      p.pos <- current_pos;
      if is_curry
      then (
        let curried =
          match consume p with
          | Token.Ident s -> s
          | _ -> assert false
        in
        expect p Token.Back_arrow |> ignore;
        let input =
          parse_list p [ Token.Semi_colon; Token.Eof; Token.Let ] (fun p ->
            parse_expression p |> get_ok_or_fail)
        in
        Ok (Ast.Decl_stmt (Ast.Curry_decl { tags; name; curried; input })))
      else (
        let params =
          if peek p = Token.Eql || peek p = Token.Skinny_arrow
          then []
          else if peek p = Token.Open_paren
          then (
            advance p;
            expect p Token.Close_paren |> ignore;
            [])
          else parse_list p [ Token.Eql; Token.Skinny_arrow ] parse_decl_param
        in
        let explicit_ret =
          if peek p = Token.Skinny_arrow
          then (
            advance p;
            Some (parse_type p))
          else None
        in
        let body =
          if peek p = Token.Eql || peek p = Token.Walrus
          then (
            advance p;
            parse_body p)
          else [], None
        in
        Ok (Ast.Decl_stmt (Ast.Decl { tags; name; params; explicit_ret; body }))))
  | Token.Back_arrow ->
    advance p;
    let curried =
      match consume p with
      | Token.Ident s -> s
      | _ -> failwith "Expected identifier for currying"
    in
    let input =
      parse_list p [ Token.Semi_colon; Token.Eof; Token.Let ] (fun p ->
        parse_expression p |> get_ok_or_fail)
    in
    Ok (Ast.Decl_stmt (Ast.Curry_decl { tags; name; curried; input }))
  | Token.Colon ->
    (* Variable with type, e.g. let x : i32 = 5 *)
    advance p;
    let explicit_ret = Some (parse_type p) in
    let body =
      if peek p = Token.Eql || peek p = Token.Walrus
      then (
        advance p;
        parse_body p)
      else [], None
    in
    Ok (Ast.Decl_stmt (Ast.Decl { tags; name; params = []; explicit_ret; body }))
  | Token.Eql | Token.Walrus ->
    (* Inferred variable, e.g. let x = 5 *)
    advance p;
    let body = parse_body p in
    Ok (Ast.Decl_stmt (Ast.Decl { tags; name; params = []; explicit_ret = None; body }))
  | _ -> Error (Printf.sprintf "Unexpected token %s in declaration" (Token.show (peek p)))

and parse_decl_param p =
  let is_optional = peek p = Token.Question in
  if is_optional then advance p;
  let name =
    match consume p with
    | Token.Ident s -> s
    | t ->
      failwith (Printf.sprintf "Expected identifier for parameter, got %s" (Token.show t))
  in
  if peek p = Token.Ellipsis
  then (
    advance p;
    Ast.Variadic name)
  else (
    let param_type, default_value =
      if peek p = Token.Colon
      then (
        advance p;
        let t = parse_type p in
        let default =
          if is_optional && peek p = Token.Eql
          then (
            advance p;
            Some (parse_expression p |> get_ok_or_fail))
          else None
        in
        Some t, default)
      else (
        let default =
          if is_optional && peek p = Token.Eql
          then (
            advance p;
            Some (parse_expression p |> get_ok_or_fail))
          else None
        in
        None, default)
    in
    match is_optional, param_type, default_value with
    | false, Some t, None -> Ast.Typed (name, t)
    | false, None, None -> Ast.Untyped name
    | true, Some t, Some e -> Ast.OptionalTyped (name, t, e)
    | true, None, Some e -> Ast.OptionalUntyped (name, e)
    | _ -> failwith "Invalid parameter declaration")

and parse_type p =
  match consume p with
  | Token.Ident s -> Ast.User s
  | Token.Open_paren ->
    expect p Token.Close_paren |> ignore;
    Ast.Unit_typ
  | _ -> failwith "Invalid type"

and parse_body p =
  if peek p = Token.Open_brack
  then (
    advance p;
    let stmts = ref [] in
    let expr = ref None in
    while peek p <> Token.Close_brack do
      if peek p = Token.Eof then failwith "Unclosed block";
      (* This is a bit of a hack to see if the next thing is an expression that is the last thing in the block *)
      let current_pos = p.pos in
      let is_last =
        try
          match parse_expression p with
          | Ok _ -> peek p = Token.Close_brack
          | Error _ -> false
        with
        | _ -> false
      in
      p.pos <- current_pos;
      if is_last
      then expr := Some (parse_expression p |> get_ok_or_fail)
      else (
        match parse_statement p with
        | Ok stmt ->
          stmts := stmt :: !stmts;
          if peek p = Token.Semi_colon then advance p
        | Error err -> failwith err)
    done;
    expect p Token.Close_brack |> ignore;
    List.rev !stmts, !expr)
  else (
    let expr = parse_expression p |> get_ok_or_fail in
    [], Some expr)

and parse_return_stmt p =
  expect p Token.Return |> ignore;
  if peek p = Token.Semi_colon || is_at_end p
  then Ok (Ast.Return_stmt Ast.Naked)
  else (
    let expr = parse_expression p |> get_ok_or_fail in
    Ok (Ast.Return_stmt (With_expr expr)))

and parse_if_stmt p =
  expect p Token.If |> ignore;
  let cond = parse_expression p |> get_ok_or_fail in
  let body = parse_body_as_t_list p in
  let elif = parse_else_stmt p in
  Ok (Ast.If_stmt { cond; body; elif })

and parse_else_stmt p =
  if peek p = Token.Else
  then (
    advance p;
    if peek p = Token.If
    then (
      advance p;
      let cond = parse_expression p |> get_ok_or_fail in
      let body = parse_body_as_t_list p in
      let elif = parse_else_stmt p in
      Ast.Else_if (cond, body, elif))
    else (
      let body = parse_body_as_t_list p in
      Ast.Else body))
  else Ast.Nope

and parse_body_as_t_list p =
  if peek p = Token.Open_brack
  then (
    advance p;
    let stmts = ref [] in
    while peek p <> Token.Close_brack do
      if peek p = Token.Eof then failwith "Unclosed block";
      match parse_statement p with
      | Ok stmt -> stmts := Ast.Statement stmt :: !stmts
      | Error err -> failwith err
    done;
    expect p Token.Close_brack |> ignore;
    List.rev !stmts)
  else (
    match parse_statement p with
    | Ok stmt -> [ Ast.Statement stmt ]
    | Error err -> failwith err)

and parse_expression_stmt p =
  match parse_expression p with
  | Ok expr ->
    if peek p = Token.Semi_colon then advance p;
    Ok (Ast.Expression_stmt expr)
  | Error err -> Error err

and parse_expression p =
  match peek p with
  | Token.Struct -> parse_struct_expr p
  | Token.Enum -> parse_enum_expr p
  | Token.Macro -> parse_macro_expr p
  | Token.Derive -> parse_derive_expr p
  | Token.Match -> parse_match_expr p
  | _ -> parse_relational_expr p

and parse_match_expr p =
  expect p Token.Match |> ignore;
  let target = parse_expression p |> get_ok_or_fail in
  expect p Token.With |> ignore;
  let arms = ref [] in
  while peek p = Token.Bar do
    advance p;
    let param = parse_match_param p in
    let if_opt =
      if peek p = Token.If
      then (
        advance p;
        Some (parse_expression p |> get_ok_or_fail))
      else None
    in
    expect p Token.Skinny_arrow |> ignore;
    let body = parse_match_arm_body p in
    arms := (param, if_opt, body) :: !arms
  done;
  Ok (Ast.Match_expr (target, List.rev !arms))

and parse_match_param p =
  (* Try to parse a pattern. For now, we'll reuse primary expression parsing. *)
  let e = parse_primary p |> get_ok_or_fail in
  match e with
  | Ast.Unary_val (Ast.Implicit_member _) ->
    (match peek p with
     | Token.Ident _ ->
       let var = parse_primary p |> get_ok_or_fail in
       Ast.Single
         (wrap_unary
            (Ast.Unary_call
               (Ast.Decl_call (wrap_unary e, [ Ast.Positional (wrap_unary var) ]))))
     | _ -> Ast.Single (wrap_unary e))
  | Ast.Unary_call (Ast.Decl_call (target, params)) ->
    (* Check if it's .Variant(x) *)
    Ast.Single (wrap_unary (Ast.Unary_call (Ast.Decl_call (target, params))))
  | _ -> Ast.Single (wrap_unary e)

and parse_match_arm_body p =
  if peek p = Token.Open_brack
  then parse_body p
  else (
    let e = parse_expression p |> get_ok_or_fail in
    [], Some e)

and parse_with_block p =
  if peek p = Token.With
  then (
    advance p;
    expect p Token.Open_brack |> ignore;
    let stmts = ref [] in
    while peek p <> Token.Close_brack && not (is_at_end p) do
      match parse_toplevel p with
      | Ok stmt ->
        stmts := stmt :: !stmts;
        if peek p = Token.Semi_colon then advance p
      | Error err -> failwith err
    done;
    expect p Token.Close_brack |> ignore;
    Some (List.rev !stmts))
  else None

and parse_struct_expr p =
  expect p Token.Struct |> ignore;
  expect p Token.Open_brack |> ignore;
  let fields = parse_list p [ Token.Close_brack ] parse_struct_field in
  expect p Token.Close_brack |> ignore;
  let with_blk = parse_with_block p in
  Ok (Ast.Struct_expr (fields, with_blk))

and parse_struct_field p =
  let name =
    match consume p with
    | Token.Ident s -> s
    | _ -> failwith "expected ident"
  in
  expect p Token.Colon |> ignore;
  let typ = parse_type p in
  let expr =
    if peek p = Token.Eql
    then (
      advance p;
      Some (parse_expression p |> get_ok_or_fail))
    else None
  in
  name, typ, expr

and parse_enum_expr p =
  expect p Token.Enum |> ignore;
  expect p Token.Open_brack |> ignore;
  let variants = parse_list p [ Token.Close_brack ] parse_enum_variant in
  expect p Token.Close_brack |> ignore;
  let with_blk = parse_with_block p in
  Ok (Ast.Enum_expr (variants, with_blk))

and parse_enum_variant p =
  let name =
    match consume p with
    | Token.Ident s -> s
    | _ -> failwith "expected ident"
  in
  let body =
    if peek p = Token.Skinny_arrow
    then (
      advance p;
      if peek p = Token.Struct
      then (
        advance p;
        expect p Token.Open_brack |> ignore;
        let fields = parse_list p [ Token.Close_brack ] parse_struct_field in
        expect p Token.Close_brack |> ignore;
        Some (Ast.Struct_body fields))
      else Some (Ast.Type_body (parse_type p)))
    else None
  in
  name, body

and parse_macro_expr p =
  expect p Token.Macro |> ignore;
  expect p Token.Open_brack |> ignore;
  let stmts = ref [] in
  while peek p <> Token.Close_brack && not (is_at_end p) do
    match parse_toplevel p with
    | Ok stmt ->
      stmts := stmt :: !stmts;
      if peek p = Token.Semi_colon then advance p
    | Error err -> failwith err
  done;
  expect p Token.Close_brack |> ignore;
  Ok (Ast.Macro_expr (List.rev !stmts))

and parse_derive_expr p =
  expect p Token.Derive |> ignore;
  expect p Token.Open_brack |> ignore;
  let stmts = ref [] in
  while peek p <> Token.Close_brack && not (is_at_end p) do
    match parse_toplevel p with
    | Ok stmt ->
      stmts := stmt :: !stmts;
      if peek p = Token.Semi_colon then advance p
    | Error err -> failwith err
  done;
  expect p Token.Close_brack |> ignore;
  Ok (Ast.Derive_expr (List.rev !stmts))

and parse_relational_expr p =
  let left = parse_additive_expr p |> get_ok_or_fail in
  match peek p with
  | Token.Double_eql ->
    advance p;
    Ok (Ast.Relational_expr (Ast.Eql (left, parse_additive_expr p |> get_ok_or_fail)))
  | Token.Lesser_eql ->
    advance p;
    Ok (Ast.Relational_expr (Ast.Leq (left, parse_additive_expr p |> get_ok_or_fail)))
  | Token.Greater_eql ->
    advance p;
    Ok (Ast.Relational_expr (Ast.Geq (left, parse_additive_expr p |> get_ok_or_fail)))
  | Token.Lesser ->
    advance p;
    Ok (Ast.Relational_expr (Ast.Lt (left, parse_additive_expr p |> get_ok_or_fail)))
  | Token.Greater ->
    advance p;
    Ok (Ast.Relational_expr (Ast.Gt (left, parse_additive_expr p |> get_ok_or_fail)))
  | _ -> Ok (Ast.Relational_expr (Ast.Relational_val left))

and parse_additive_expr p =
  let rec aux left =
    match peek p with
    | Token.Plus ->
      advance p;
      aux (Ast.Add (left, parse_multiplicative_expr p |> get_ok_or_fail))
    | Token.Dash ->
      advance p;
      aux (Ast.Sub (left, parse_multiplicative_expr p |> get_ok_or_fail))
    | _ -> left
  in
  Ok (aux (Ast.Additive_val (parse_multiplicative_expr p |> get_ok_or_fail)))

and parse_multiplicative_expr p =
  let rec aux left =
    match peek p with
    | Token.Star ->
      advance p;
      aux (Ast.Mul (left, parse_unary_expr p |> get_ok_or_fail))
    | Token.Forward_slash ->
      advance p;
      aux (Ast.Div (left, parse_unary_expr p |> get_ok_or_fail))
    | Token.Mod ->
      advance p;
      aux (Ast.Mod (left, parse_unary_expr p |> get_ok_or_fail))
    | Token.Carrot ->
      advance p;
      aux (Ast.Pow (left, parse_unary_expr p |> get_ok_or_fail))
    | _ -> left
  in
  Ok (aux (Ast.Multiplicative_val (parse_unary_expr p |> get_ok_or_fail)))

and parse_unary_expr p =
  match peek p with
  | Token.Dash ->
    advance p;
    Ok (Ast.Neg (parse_unary_expr p |> get_ok_or_fail))
  | Token.Bang ->
    advance p;
    Ok (Ast.Not (parse_unary_expr p |> get_ok_or_fail))
  | _ -> parse_primary p

and parse_primary p =
  let rec aux left =
    match peek p with
    | Token.Dot ->
      advance p;
      (match consume p with
       | Token.Ident member -> aux (Ast.Unary_member (left, member))
       | _ -> failwith "Expected identifier after .")
    | Token.Bang ->
      advance p;
      if peek p = Token.Open_paren
      then (
        advance p;
        let params = parse_list p [ Token.Close_paren ] parse_call_param in
        expect p Token.Close_paren |> ignore;
        let call = Ast.Macro_call (wrap_unary left, params) in
        aux (Ast.Unary_call call))
      else Ast.Not (aux left) (* This is a bit weird but Bang can be Not or Macro *)
    | Token.Open_paren ->
      advance p;
      let params = parse_list p [ Token.Close_paren ] parse_call_param in
      expect p Token.Close_paren |> ignore;
      let call = Ast.Decl_call (wrap_unary left, params) in
      aux (Ast.Unary_call call)
    | _ -> left
  in
  let res =
    match peek p with
    | Token.Ident name ->
      advance p;
      Ast.Unary_val (Ast.Ident name)
    | Token.Dot ->
      advance p;
      (match consume p with
       | Token.Ident name -> Ast.Unary_val (Ast.Implicit_member name)
       | _ -> failwith "Expected identifier after .")
    | _ ->
      (match parse_atom p with
       | Ok atom -> Ast.Unary_val atom
       | Error err -> failwith err)
  in
  Ok (aux res)

and parse_call_expr_with_name name p =
  let is_macro = peek p = Token.Bang in
  if is_macro then advance p;
  expect p Token.Open_paren |> ignore;
  let params = parse_list p [ Token.Close_paren ] parse_call_param in
  expect p Token.Close_paren |> ignore;
  let target = Ast.Unary_val (Ast.Ident name) in
  if is_macro
  then Ast.Macro_call (wrap_unary target, params)
  else Ast.Decl_call (wrap_unary target, params)

and parse_call_param p =
  if peek p = Token.Tilde
  then (
    advance p;
    let name =
      match consume p with
      | Token.Ident s -> s
      | _ -> failwith "expected identifier for named parameter"
    in
    expect p Token.Eql |> ignore;
    let expr = parse_expression p |> get_ok_or_fail in
    Ast.Named (name, expr))
  else (
    let expr = parse_expression p |> get_ok_or_fail in
    Ast.Positional expr)

and parse_atom p =
  match consume p with
  | Token.Number n -> Ok (Ast.Int n)
  | Token.String s -> Ok (Ast.String s)
  | Token.Char c -> Ok (Ast.Char c)
  | Token.True -> Ok (Ast.Bool true)
  | Token.False -> Ok (Ast.Bool false)
  | Token.Ident s -> Ok (Ast.Ident s)
  | Token.Open_paren ->
    if peek p = Token.Close_paren
    then (
      advance p;
      Ok Ast.Unit_val)
    else (
      let expr = parse_expression p |> get_ok_or_fail in
      expect p Token.Close_paren |> ignore;
      Ok (Ast.Grouping expr))
  | t ->
    Error
      (Printf.sprintf "Unexpected token %s at %s" (Token.show t) (Token.show_loc (loc p)))

and parse_list : 'a. t -> Token.t list -> (t -> 'a) -> 'a list =
  fun p untils f ->
  let rec aux acc =
    if List.mem (peek p) untils || is_at_end p
    then acc
    else (
      let res = f p in
      if peek p = Token.Comma then advance p;
      aux (res :: acc))
  in
  List.rev (aux [])
;;
