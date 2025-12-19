open Node
open Token

exception Parser_unhandled of string
exception Parser_expected of string
exception Parser_expected_many of string list
exception End_of_params
exception Parsing_error of string

let errno ~msg loc t =
  Format.sprintf "%s:%d:%d: %s: %s" loc.file loc.row loc.col msg (Token.show t)
;;

let rec parse ts = parse' (List.split ts)

and parse' = function
  | [], [] -> []
  | [ Eof ], _ -> []
  | Open :: ts, _ :: ls ->
    (try
       let stmt, (ts', ls') = parse_open_statement ts ls in
       stmt :: parse' (ts', ls')
     with
     | Parsing_error msg ->
       let rec advance = function
         | Semi_colon :: ts, _ :: ls -> ts, ls
         | _ :: ts, _ :: ls -> advance (ts, ls)
         | _ -> ts, ls
       in
       let ts', ls' = advance (ts, ls) in
       Error msg :: parse' (ts', ls'))
  | t :: ts, l :: ls ->
    (match t with
     | Close_brack -> parse' (ts, ls)
     | _ ->
       let node = Error (errno ~msg:"expected statement start but found" l t) in
       node :: parse' (ts, ls))
  | _, _ -> failwith "unreachable"

and parse_open_statement ts ls =
  let mods, (ts_m, ls_m) = parse_open_modules (ts, ls) in
  match ts_m, ls_m with
  | With :: Open_brack :: ts_w, _ :: _ :: ls_w ->
    let items, (ts_f, ls_f) = parse_items ts_w ls_w in
    Statement (Open_stmt { mods; elements = items }), (ts_f, ls_f)
  | _ -> Statement (Open_stmt { mods; elements = [] }), (ts_m, ls_m)

and parse_items ts ls =
  match ts, ls with
  | Close_brack :: ts_r, _ :: ls_r -> [], (ts_r, ls_r)
  | [], l :: _ ->
    raise (Parsing_error (errno ~msg:"unclosed brace in with clause" l Token.Eof))
  | _ ->
    let item, (ts_i, ls_i) = parse_item ts ls in
    (match ts_i, ls_i with
     | Comma :: ts_c, _ :: ls_c ->
       (match ts_c, ls_c with
        | Close_brack :: ts_r, _ :: ls_r -> [ item ], (ts_r, ls_r)
        | _ ->
          let rest, state = parse_items ts_c ls_c in
          item :: rest, state)
     | Close_brack :: _, _ -> [ item ], (ts_i, ls_i)
     | t :: _, l :: _ ->
       raise (Parsing_error (errno ~msg:"expected comma or closing brace but found" l t))
     | _, _ -> failwith "unreachable")

and parse_item ts ls =
  let rec get_path ts ls =
    match ts, ls with
    | Ident i :: Dot :: ts_r, _ :: _ :: ls_r ->
      let path, state = get_path ts_r ls_r in
      i :: path, state
    | Ident i :: ts_r, _ :: ls_r -> [ i ], (ts_r, ls_r)
    | t :: _, l :: _ ->
      raise (Parsing_error (errno ~msg:"expected identifier in path but found" l t))
    | [], l :: _ ->
      raise (Parsing_error (errno ~msg:"unexpected end of path" l Token.Eof))
    | _, _ -> failwith "unreachable"
  in
  let path, (ts_p, ls_p) = get_path ts ls in
  match ts_p, ls_p with
  | As :: Ident alias :: ts_a, _ :: _ :: ls_a -> { path; alias = Some alias }, (ts_a, ls_a)
  | As :: t :: _ts_a, l :: _ls_a ->
    raise (Parsing_error (errno ~msg:"expected identifier after 'as' but found" l t))
  | Comma :: _, _ -> { path; alias = None }, (ts_p, ls_p)
  | Close_brack :: _, _ -> { path; alias = None }, (ts_p, ls_p)
  | _ -> { path; alias = None }, (ts_p, ls_p)

and parse_open_modules = function
  | Ident m1 :: Dot :: ts, ls ->
    let rest, (ts', ls') = parse_open_modules (ts, ls) in
    m1 :: rest, (ts', ls')
  | Ident m1 :: ts, ls -> [ m1 ], (ts, ls)
  | t :: _, l :: _ ->
    raise (Parsing_error (errno ~msg:"expected module identifier but found" l t))
  | [], l :: _ ->
    raise
      (Parsing_error (errno ~msg:"unexpected end of input in module path" l Token.Eof))
  | _, _ -> failwith "unreachable"
;;
