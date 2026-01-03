(* Preprocessor for macro expansion *)

(* Macro table: maps macro names to their bodies *)
let macro_table : (string, Ast.t list) Hashtbl.t = Hashtbl.create 16

(* Collect macro definitions from AST *)
let rec collect_macros = function
  | [] -> ()
  | Ast.Statement stmt :: rest ->
    (match stmt with
     | Ast.Decl_stmt (Ast.Decl { name; body = _stmts, Some (Ast.Macro_expr body); _ }) ->
       Hashtbl.replace macro_table name body
     | _ -> ());
    collect_macros rest
  | _ :: rest -> collect_macros rest
;;

let wrap_unary (u : Ast.unary_expr) : Ast.expression =
  Ast.Relational_expr (Ast.Relational_val (Ast.Additive_val (Ast.Multiplicative_val u)))
;;

let string_expr s = wrap_unary (Ast.Unary_val (Ast.String s))
let ident_expr name = wrap_unary (Ast.Unary_val (Ast.Ident name))

let ensure_trailing_newline s =
  let len = String.length s in
  if len > 0 && s.[len - 1] = '\n' then s else s ^ "\n"
;;

let rec expand_nodes nodes = List.map expand_t nodes

and expand_t = function
  | Ast.Statement stmt -> Ast.Statement (expand_statement stmt)
  | Ast.Expression expr -> Ast.Expression (expand_expression expr)
  | Ast.Error _ as e -> e

and expand_statement = function
  | Ast.Open_stmt o -> Ast.Open_stmt o
  | Ast.Decl_stmt decl -> Ast.Decl_stmt (expand_decl decl)
  | Ast.Return_stmt (Ast.With_expr expr) ->
    Ast.Return_stmt (Ast.With_expr (expand_expression expr))
  | Ast.Return_stmt Ast.Naked as r -> r
  | Ast.If_stmt { cond; body; elif } ->
    Ast.If_stmt
      { cond = expand_expression cond; body = expand_nodes body; elif = expand_else elif }
  | Ast.While_stmt { cond; body } ->
    Ast.While_stmt { cond = expand_expression cond; body = expand_nodes body }
  | Ast.For_stmt (Ast.For_iter { var; iterable; body }) ->
    Ast.For_stmt
      (Ast.For_iter
         { var; iterable = expand_expression iterable; body = expand_nodes body })
  | Ast.For_stmt (Ast.For_c { var; init; cond; update; body }) ->
    Ast.For_stmt
      (Ast.For_c
         { var
         ; init = expand_expression init
         ; cond = expand_expression cond
         ; update = expand_expression update
         ; body = expand_nodes body
         })
  | Ast.For_stmt (Ast.For_tuple { vars; iterable; body }) ->
    Ast.For_stmt
      (Ast.For_tuple
         { vars; iterable = expand_expression iterable; body = expand_nodes body })
  | Ast.Expression_stmt expr -> Ast.Expression_stmt (expand_expression expr)

and expand_else = function
  | Ast.Else_if (cond, body, rest) ->
    Ast.Else_if (expand_expression cond, expand_nodes body, expand_else rest)
  | Ast.Else body -> Ast.Else (expand_nodes body)
  | Ast.Nope as e -> e

and expand_decl = function
  | Ast.Decl ({ body = stmts, expr_opt; _ } as decl) ->
    let stmts' = List.map expand_statement stmts in
    let expr_opt' = Option.map expand_expression expr_opt in
    Ast.Decl { decl with body = stmts', expr_opt' }
  | Ast.Curry_decl ({ input; _ } as decl) ->
    Ast.Curry_decl { decl with input = List.map expand_expression input }
  | Ast.Import_decl _ as d -> d
  | Ast.Module_decl ({ body; _ } as m) ->
    Ast.Module_decl { m with body = expand_nodes body }
  | Ast.Export_stmt _ as e -> e

and expand_expression = function
  | Ast.Call_expr call -> Ast.Call_expr (expand_call_expr call)
  | Ast.Relational_expr rel -> Ast.Relational_expr (expand_relational rel)
  | Ast.Assignment_expr assign -> Ast.Assignment_expr (expand_assignment assign)
  | Ast.List_expr elems -> Ast.List_expr (List.map expand_expression elems)
  | Ast.Match_expr (expr, arms) ->
    Ast.Match_expr (expand_expression expr, List.map expand_match_arm arms)
  | Ast.Struct_expr (fields, with_block) ->
    Ast.Struct_expr
      (List.map expand_struct_field fields, Option.map expand_nodes with_block)
  | Ast.Enum_expr (variants, with_block) ->
    Ast.Enum_expr
      (List.map expand_enum_variant variants, Option.map expand_nodes with_block)
  | Ast.Macro_expr body -> Ast.Macro_expr (expand_nodes body)
  | Ast.Derive_expr body -> Ast.Derive_expr (expand_nodes body)

and expand_assignment = function
  | Ast.Add_assign (name, expr) -> Ast.Add_assign (name, expand_additive expr)
  | Ast.Sub_assign (name, expr) -> Ast.Sub_assign (name, expand_additive expr)
  | Ast.Mul_assign (name, expr) -> Ast.Mul_assign (name, expand_additive expr)
  | Ast.Div_assign (name, expr) -> Ast.Div_assign (name, expand_additive expr)

and expand_struct_field (name, typ, expr_opt) =
  name, typ, Option.map expand_expression expr_opt

and expand_enum_variant = function
  | ident, Some (Ast.Struct_body fields) ->
    ident, Some (Ast.Struct_body (List.map expand_struct_field fields))
  | ident, Some (Ast.Type_body typ) -> ident, Some (Ast.Type_body typ)
  | ident, None -> ident, None

and expand_match_arm (param, if_opt, (stmts, expr_opt)) =
  let if_opt' = Option.map expand_expression if_opt in
  let stmts' = List.map expand_statement stmts in
  let expr_opt' = Option.map expand_expression expr_opt in
  param, if_opt', (stmts', expr_opt')

and expand_relational = function
  | Ast.Eql (l, r) -> Ast.Eql (expand_additive l, expand_additive r)
  | Ast.Neq (l, r) -> Ast.Neq (expand_additive l, expand_additive r)
  | Ast.Lt (l, r) -> Ast.Lt (expand_additive l, expand_additive r)
  | Ast.Gt (l, r) -> Ast.Gt (expand_additive l, expand_additive r)
  | Ast.Leq (l, r) -> Ast.Leq (expand_additive l, expand_additive r)
  | Ast.Geq (l, r) -> Ast.Geq (expand_additive l, expand_additive r)
  | Ast.Relational_val add -> Ast.Relational_val (expand_additive add)

and expand_additive = function
  | Ast.Add (l, r) -> Ast.Add (expand_additive l, expand_multiplicative r)
  | Ast.Sub (l, r) -> Ast.Sub (expand_additive l, expand_multiplicative r)
  | Ast.Additive_val mul -> Ast.Additive_val (expand_multiplicative mul)

and expand_multiplicative = function
  | Ast.Mul (l, r) -> Ast.Mul (expand_multiplicative l, expand_unary r)
  | Ast.Div (l, r) -> Ast.Div (expand_multiplicative l, expand_unary r)
  | Ast.Mod (l, r) -> Ast.Mod (expand_multiplicative l, expand_unary r)
  | Ast.Pow (l, r) -> Ast.Pow (expand_multiplicative l, expand_unary r)
  | Ast.Multiplicative_val u -> Ast.Multiplicative_val (expand_unary u)

and expand_unary = function
  | Ast.Neg u -> Ast.Neg (expand_unary u)
  | Ast.Not u -> Ast.Not (expand_unary u)
  | Ast.Unary_member (u, field) -> Ast.Unary_member (expand_unary u, field)
  | Ast.Unary_call call -> Ast.Unary_call (expand_call_expr call)
  | Ast.Unary_val atom -> Ast.Unary_val (expand_atom atom)

and expand_atom = function
  | Ast.Grouping expr -> Ast.Grouping (expand_expression expr)
  | atom -> atom

and expand_call_expr = function
  | Ast.Decl_call (callee, params) ->
    Ast.Decl_call (expand_expression callee, List.map expand_call_param params)
  | Ast.Macro_call (callee, params) ->
    let callee' = expand_expression callee in
    let params' = List.map expand_call_param params in
    (match callee' with
     | Ast.Relational_expr
         (Ast.Relational_val
            (Ast.Additive_val
               (Ast.Multiplicative_val (Ast.Unary_val (Ast.Ident "println"))))) ->
       Ast.Decl_call (ident_expr "printf", expand_println_params params')
     | _ -> Ast.Macro_call (callee', params'))

and expand_call_param = function
  | Ast.Named (name, expr) -> Ast.Named (name, expand_expression expr)
  | Ast.Positional expr -> Ast.Positional (expand_expression expr)

and expand_println_params = function
  | Ast.Positional expr :: rest ->
    (match extract_string_literal expr with
     | Some s -> Ast.Positional (string_expr (ensure_trailing_newline s)) :: rest
     | None -> Ast.Positional expr :: rest)
  | params -> params

and extract_string_literal = function
  | Ast.Relational_expr (Ast.Relational_val add) -> extract_string_add add
  | _ -> None

and extract_string_add = function
  | Ast.Additive_val mul -> extract_string_mul mul
  | _ -> None

and extract_string_mul = function
  | Ast.Multiplicative_val u -> extract_string_unary u
  | _ -> None

and extract_string_unary = function
  | Ast.Unary_val (Ast.String s) -> Some s
  | Ast.Unary_val (Ast.Grouping expr) -> extract_string_literal expr
  | _ -> None
;;

let preprocess nodes =
  Hashtbl.reset macro_table;
  collect_macros nodes;
  expand_nodes nodes
;;
