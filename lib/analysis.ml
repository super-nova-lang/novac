module A = Ast

(** Analysis result types *)
type analysis_error =
  | Undefined_variable of string * string list
  | Type_mismatch of A.typ * A.typ
  | Duplicate_declaration of string * (int * int)
  | Invalid_operation of string
  | Missing_return_type of string

type analysis_warning =
  | Unused_variable of string
  | Shadowed_variable of string

type analysis_result =
  | Error of analysis_error
  | Warning of analysis_warning

(** Symbol table entry *)
type symbol_info =
  { name : string
  ; declared_typ : A.typ option (* Explicitly declared type *)
  ; inferred_typ : A.typ option ref (* Inferred type *)
  ; mutable used : bool
  ; scope_level : int
  ; location : int * int (* line, column *)
  }

(** Symbol table *)
type symbol_table = (string, symbol_info) Hashtbl.t

(** Analysis context *)
type context =
  { symbols : symbol_table
  ; scope_level : int
  ; errors : analysis_result list ref
  ; warnings : analysis_result list ref
  }

let take n lst =
  let rec aux i acc = function
    | _ when i = 0 -> List.rev acc
    | [] -> List.rev acc
    | x :: xs -> aux (i - 1) (x :: acc) xs
  in
  aux n [] lst
;;

let suggest_names ctx name =
  if String.length name = 0
  then []
  else
    Hashtbl.to_seq_keys ctx.symbols
    |> Seq.filter (fun cand ->
      cand <> name && String.length cand > 0 && String.get cand 0 = String.get name 0)
    |> List.of_seq
    |> List.sort String.compare
    |> take 3
;;

(** Create a new analysis context *)
let create_context () =
  { symbols = Hashtbl.create 100; scope_level = 0; errors = ref []; warnings = ref [] }
;;

(** Enter a new scope *)
let enter_scope ctx = { ctx with scope_level = ctx.scope_level + 1 }

(* Exit current scope and check for unused variables. Only warn for locals (scope > 0). *)
let exit_scope ctx =
  Hashtbl.iter
    (fun name info ->
       if (not info.used) && info.scope_level = ctx.scope_level && ctx.scope_level > 0
       then ctx.warnings := Warning (Unused_variable name) :: !(ctx.warnings))
    ctx.symbols;
  Hashtbl.filter_map_inplace
    (fun _ (info : symbol_info) ->
       if info.scope_level < ctx.scope_level then Some info else None)
    ctx.symbols;
  { ctx with scope_level = ctx.scope_level - 1 }
;;

(** Add a symbol to the current scope *)
let add_symbol ctx name typ location =
  (* Check if symbol already exists in current scope *)
  let existing = Hashtbl.find_opt ctx.symbols name in
  match existing with
  | Some info when info.scope_level = ctx.scope_level ->
    ctx.errors := Error (Duplicate_declaration (name, location)) :: !(ctx.errors)
  | _ ->
    let info =
      { name
      ; declared_typ = typ
      ; inferred_typ = ref None
      ; used = false
      ; scope_level = ctx.scope_level
      ; location
      }
    in
    Hashtbl.add ctx.symbols name info
;;

(** Look up a symbol (find the one with highest scope level) *)
let lookup_symbol ctx name = Hashtbl.find_opt ctx.symbols name

(** Set inferred type for a symbol *)
let set_inferred_type ctx name typ =
  match lookup_symbol ctx name with
  | Some info -> info.inferred_typ := Some typ
  | None -> ()
;;

(** Get the effective type of a symbol (declared or inferred) *)
let get_symbol_type info =
  match info.declared_typ with
  | Some t -> t
  | None ->
    (match !(info.inferred_typ) with
     | Some t -> t
     | None -> A.Unit_typ (* Default to unit if no type info *))
;;

(** Type inference for expressions *)
let rec infer_expression_type ctx expr =
  match expr with
  | A.Call_expr call -> infer_call_type ctx call
  | A.Relational_expr rel -> infer_relational_type ctx rel
  | A.Match_expr match_expr -> infer_match_type ctx match_expr
  | A.Struct_expr _ -> A.User "struct" (* TODO: proper struct types *)
  | A.Enum_expr _ -> A.User "enum" (* TODO: proper enum types *)
  | A.Macro_expr _ -> A.Unit_typ (* Macros don't have types *)
  | A.Derive_expr _ -> A.Unit_typ (* Derive doesn't have types *)

and infer_call_type ctx = function
  | A.Decl_call (_expr, params) ->
    (* For now, assume function call returns unit *)
    List.iter (infer_call_param_type ctx) params;
    A.Unit_typ
  | A.Macro_call (_expr, params) ->
    List.iter (infer_call_param_type ctx) params;
    A.Unit_typ

and infer_call_param_type ctx = function
  | A.Named (_, expr) -> ignore (infer_expression_type ctx expr)
  | A.Positional expr -> ignore (infer_expression_type ctx expr)

and infer_relational_type ctx = function
  | A.Eql (left, right)
  | A.Neq (left, right)
  | A.Lt (left, right)
  | A.Gt (left, right)
  | A.Leq (left, right)
  | A.Geq (left, right) ->
    ignore (infer_additive_type ctx left);
    ignore (infer_additive_type ctx right);
    A.User "bool" (* Relational ops return bool *)
  | A.Relational_val expr -> infer_additive_type ctx expr

and infer_additive_type ctx = function
  | A.Add (left, right) | A.Sub (left, right) ->
    ignore (infer_additive_type ctx left);
    ignore (infer_multiplicative_type ctx right);
    A.User "i32" (* Assume arithmetic ops on i32 *)
  | A.Additive_val expr -> infer_multiplicative_type ctx expr

and infer_multiplicative_type ctx = function
  | A.Mul (left, right) | A.Div (left, right) | A.Mod (left, right) | A.Pow (left, right)
    ->
    ignore (infer_multiplicative_type ctx left);
    ignore (infer_unary_type ctx right);
    A.User "i32"
  | A.Multiplicative_val expr -> infer_unary_type ctx expr

and infer_unary_type ctx = function
  | A.Neg expr | A.Not expr ->
    ignore (infer_unary_type ctx expr);
    A.User "i32" (* Assume int for now *)
  | A.Unary_member (expr, _) ->
    ignore (infer_unary_type ctx expr);
    A.Unit_typ (* TODO: member access type *)
  | A.Unary_call call -> infer_call_type ctx call
  | A.Unary_val atom -> infer_atom_type ctx atom

and infer_atom_type ctx = function
  | A.String _ -> A.User "string"
  | A.Bool _ -> A.User "bool"
  | A.Char _ -> A.User "char"
  | A.Int _ -> A.User "i32"
  | A.Ident "_" -> A.User "i32" (* wildcard, arbitrary type *)
  | A.Ident name ->
    (match lookup_symbol ctx name with
     | Some info ->
       info.used <- true;
       get_symbol_type info
     | None ->
       let suggestions = suggest_names ctx name in
       ctx.errors := Error (Undefined_variable (name, suggestions)) :: !(ctx.errors);
       A.Unit_typ)
  | A.Implicit_member name ->
    (match lookup_symbol ctx name with
     | Some info ->
       info.used <- true;
       get_symbol_type info
     | None ->
       let suggestions = suggest_names ctx name in
       ctx.errors := Error (Undefined_variable (name, suggestions)) :: !(ctx.errors);
       A.Unit_typ)
  | A.Grouping expr -> infer_expression_type ctx expr
  | A.Unit_val -> A.Unit_typ

and infer_match_type _ctx (_expr, _arms) =
  (* TODO: infer type from match arms *)
  A.Unit_typ
;;

(** Mark a symbol as used *)
let mark_used ctx name =
  match lookup_symbol ctx name with
  | Some info -> info.used <- true
  | None -> ()
;;

(** Analyze an expression *)
let rec analyze_expression ctx expr =
  ignore (infer_expression_type ctx expr);
  match expr with
  | A.Call_expr call -> analyze_call ctx call
  | A.Relational_expr rel -> analyze_relational ctx rel
  | A.Match_expr match_expr -> analyze_match ctx match_expr
  | A.Struct_expr (fields, with_block) -> analyze_struct ctx fields with_block
  | A.Enum_expr (variants, with_block) -> analyze_enum ctx variants with_block
  | A.Macro_expr body -> analyze_macro ctx body
  | A.Derive_expr body -> analyze_derive ctx body

and analyze_call ctx = function
  | A.Decl_call (expr, params) ->
    analyze_expression ctx expr;
    List.iter (analyze_call_param ctx) params
  | A.Macro_call (expr, params) ->
    analyze_expression ctx expr;
    List.iter (analyze_call_param ctx) params

and analyze_call_param ctx = function
  | A.Named (_name, expr) -> analyze_expression ctx expr
  | A.Positional expr -> analyze_expression ctx expr

and analyze_relational ctx = function
  | A.Eql (left, right)
  | A.Neq (left, right)
  | A.Lt (left, right)
  | A.Gt (left, right)
  | A.Leq (left, right)
  | A.Geq (left, right) ->
    analyze_additive ctx left;
    analyze_additive ctx right
  | A.Relational_val expr -> analyze_additive ctx expr

and analyze_additive ctx = function
  | A.Add (left, right) | A.Sub (left, right) ->
    analyze_additive ctx left;
    analyze_multiplicative ctx right
  | A.Additive_val expr -> analyze_multiplicative ctx expr

and analyze_multiplicative ctx = function
  | A.Mul (left, right) | A.Div (left, right) | A.Mod (left, right) | A.Pow (left, right)
    ->
    analyze_multiplicative ctx left;
    analyze_unary ctx right
  | A.Multiplicative_val expr -> analyze_unary ctx expr

and analyze_unary ctx = function
  | A.Neg expr | A.Not expr -> analyze_unary ctx expr
  | A.Unary_member (expr, _) -> analyze_unary ctx expr
  | A.Unary_call call -> analyze_call ctx call
  | A.Unary_val atom -> analyze_atom ctx atom

and analyze_atom ctx = function
  | A.Ident "_" -> () (* wildcard *)
  | A.Ident name ->
    (match lookup_symbol ctx name with
     | Some _ -> mark_used ctx name
     | None ->
       let suggestions = suggest_names ctx name in
       ctx.errors := Error (Undefined_variable (name, suggestions)) :: !(ctx.errors))
  | A.Implicit_member name ->
    (match lookup_symbol ctx name with
     | Some _ -> mark_used ctx name
     | None ->
       let suggestions = suggest_names ctx name in
       ctx.errors := Error (Undefined_variable (name, suggestions)) :: !(ctx.errors))
  | A.Grouping expr -> analyze_expression ctx expr
  | _ -> () (* literals don't need analysis *)

and analyze_match ctx (expr, arms) =
  analyze_expression ctx expr;
  List.iter (analyze_match_arm ctx) arms

and analyze_match_arm ctx (param, match_if, body) =
  let ctx' = enter_scope ctx in
  analyze_match_param ctx' param;
  (match match_if with
   | Some expr -> analyze_expression ctx' expr
   | None -> ());
  analyze_match_arm_body ctx' body;
  ignore (exit_scope ctx')

and analyze_match_param ctx = function
  | A.Single expr -> analyze_expression ctx expr
  | A.Touple exprs -> List.iter (analyze_expression ctx) exprs
  | A.Item exprs -> List.iter (analyze_expression ctx) exprs

and analyze_match_arm_body ctx (stmts, expr_opt) =
  List.iter (analyze_statement ctx) stmts;
  match expr_opt with
  | Some expr -> analyze_expression ctx expr
  | None -> ()

and analyze_struct ctx fields with_block =
  List.iter
    (fun (_fname, ftyp, expr_opt) ->
       match expr_opt with
       | Some expr ->
         analyze_expression ctx expr;
         (match expr with
          | A.Relational_expr
              (A.Relational_val
                 (A.Additive_val (A.Multiplicative_val (A.Unary_val (A.Ident id))))) ->
            set_inferred_type ctx id ftyp
          | _ -> ())
       | None -> ())
    fields;
  match with_block with
  | Some block ->
    let ctx' = enter_scope ctx in
    List.iter (analyze_ast ctx') block;
    ignore (exit_scope ctx')
  | None -> ()

and analyze_enum ctx variants with_block =
  List.iter
    (fun (_name, body_opt) ->
       match body_opt with
       | Some (A.Struct_body fields) ->
         List.iter
           (fun (_fname, ftyp, expr_opt) ->
              match expr_opt with
              | Some expr ->
                analyze_expression ctx expr;
                (match expr with
                 | A.Relational_expr
                     (A.Relational_val
                        (A.Additive_val (A.Multiplicative_val (A.Unary_val (A.Ident id)))))
                   -> set_inferred_type ctx id ftyp
                 | _ -> ())
              | None -> ())
           fields
       | _ -> ())
    variants;
  match with_block with
  | Some block ->
    let ctx' = enter_scope ctx in
    List.iter (analyze_ast ctx') block;
    ignore (exit_scope ctx')
  | None -> ()

and analyze_macro ctx body = List.iter (analyze_ast ctx) body
and analyze_derive ctx body = List.iter (analyze_ast ctx) body

(** Analyze a statement *)
and analyze_statement ctx stmt =
  match stmt with
  | A.Open_stmt open_stmt -> analyze_open ctx open_stmt
  | A.Decl_stmt decl -> analyze_decl ctx decl
  | A.Return_stmt ret -> analyze_return ctx ret
  | A.If_stmt if_stmt -> analyze_if ctx if_stmt
  | A.Expression_stmt expr -> analyze_expression ctx expr

and analyze_open ctx { A.mods; elements = _elements } =
  (* Mark the opened module as used *)
  (match mods with
   | [] -> ()
   | mod_name :: _ -> mark_used ctx mod_name);
  (* Add imported names from `open ... with { ... }` to the current scope so
    later references don’t appear undefined during analysis. *)
  List.iter
    (fun (elem : A.open_stmt_element) ->
       let local_name =
         match elem.alias with
         | Some a -> a
         | None -> List.hd (List.rev elem.path)
       in
       add_symbol ctx local_name None (0, 0))
    _elements

and analyze_decl ctx = function
  | A.Decl { tags = _tags; name; params; explicit_ret; body = stmts, expr_opt } ->
    (* Add function to symbol table *)
    add_symbol ctx name explicit_ret (0, 0);
    (* TODO: get actual location *)
    let ctx' = enter_scope ctx in
    (* Add parameters to scope *)
    List.iter
      (fun param ->
         match param with
         | A.Untyped name | A.Variadic name -> add_symbol ctx' name None (0, 0)
         | A.Typed (name, typ) -> add_symbol ctx' name (Some typ) (0, 0)
         | A.OptionalTyped (name, typ, default) ->
           add_symbol ctx' name (Some typ) (0, 0);
           analyze_expression ctx' default
         | A.OptionalUntyped (name, default) ->
           add_symbol ctx' name None (0, 0);
           analyze_expression ctx' default)
      params;
    (* Analyze body *)
    List.iter (analyze_statement ctx') stmts;
    let body_type =
      match expr_opt with
      | Some expr ->
        analyze_expression ctx' expr;
        let inferred = infer_expression_type ctx' expr in
        (match expr with
         | A.Struct_expr _ -> A.User name
         | A.Enum_expr _ -> A.User name
         | _ -> inferred)
      | None -> A.Unit_typ
    in
    (* Check return type consistency *)
    (match explicit_ret with
     | Some expected_typ when expected_typ <> body_type ->
       ctx.errors := Error (Type_mismatch (expected_typ, body_type)) :: !(ctx.errors)
     | _ -> ());
    (* Set inferred type for the function *)
    set_inferred_type ctx name body_type;
    ignore (exit_scope ctx')
  | A.Curry_decl { tags = _tags; name; curried = _curried; input } ->
    (* Add curried function *)
    add_symbol ctx name None (0, 0);
    List.iter (analyze_expression ctx) input
  | A.Import_decl { name; calling_conf = _calling_conf; link_name = _link_name } ->
    (* Add imported symbol *)
    add_symbol ctx name None (0, 0)
  | A.Module_decl { name; exports; body } ->
    (* Add module to symbol table *)
    add_symbol ctx name None (0, 0);
    (* Analyze module body *)
    List.iter (analyze_ast ctx) body;
    (* Mark exported symbols as used *)
    List.iter
      (fun export ->
         match export with
         | A.Export_ident ident | A.Export_rename (ident, _) ->
           (match lookup_symbol ctx ident with
            | Some info -> info.used <- true
            | None -> ()))
      exports
  | A.Export_stmt export ->
    (* Mark exported symbol as used *)
    (match export with
     | A.Export_ident ident | A.Export_rename (ident, _) ->
       (match lookup_symbol ctx ident with
        | Some info -> info.used <- true
        | None -> ()))

and analyze_return ctx = function
  | A.With_expr expr -> analyze_expression ctx expr
  | A.Naked -> ()

and analyze_if ctx { A.cond; body; elif } =
  analyze_expression ctx cond;
  let ctx' = enter_scope ctx in
  List.iter (analyze_ast ctx') body;
  ignore (exit_scope ctx');
  analyze_else ctx elif

and analyze_else ctx = function
  | A.Else_if (cond, body, elif) ->
    analyze_expression ctx cond;
    let ctx' = enter_scope ctx in
    List.iter (analyze_ast ctx') body;
    ignore (exit_scope ctx');
    analyze_else ctx elif
  | A.Else body ->
    let ctx' = enter_scope ctx in
    List.iter (analyze_ast ctx') body;
    ignore (exit_scope ctx')
  | A.Nope -> ()

(** Analyze an AST node *)
and analyze_ast ctx = function
  | A.Statement stmt -> analyze_statement ctx stmt
  | A.Expression expr -> analyze_expression ctx expr
  | A.Error msg -> ctx.errors := Error (Invalid_operation msg) :: !(ctx.errors)
;;

(** Check for unused variables after analysis *)
let check_unused_variables ctx =
  Hashtbl.iter
    (fun name info ->
       Printf.printf
         "Checking symbol: %s, used: %b, scope: %d\n"
         name
         info.used
         info.scope_level;
       if (not info.used) && info.scope_level > 0
       then ctx.warnings := Warning (Unused_variable name) :: !(ctx.warnings))
    ctx.symbols
;;

(** Main analysis function *)
let analyze ast_nodes =
  let ctx = create_context () in
  List.iter (analyze_ast ctx) ast_nodes;
  !(ctx.errors), !(ctx.warnings)
;;
