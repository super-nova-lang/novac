type t =
  | Statement of statement
  | Expression of expression
  | Error of string
[@@deriving show]

(* STATMENT *)
and statement =
  | Open_stmt of open_stmt
  | Decl_stmt of decl_stmt
  | Return_stmt of return_stmt
  | If_stmt of if_stmt
  | While_stmt of while_stmt
  | Expression_stmt of expression

(**
  open m1
  open m1.m2.m3
  open m1 with { e1 }
  open m1 with { e1, e2 }
*)
and open_stmt =
  { mods : string list
  ; elements : open_stmt_element list
  }

and open_stmt_element =
  { path : string list
  ; alias : string option
  }
[@@deriving show]

(**
  return expression
  return statement
  return
*)
and return_stmt =
  | With_expr of expression
  | Naked

(**
  if cond { expr1 } else_stmt
*)
and if_stmt =
  { cond : expression
  ; body : t list
  ; elif : else_stmt
  }

(**
  while cond { body }
*)
and while_stmt =
  { cond : expression
  ; body : t list
  }

(**
  else if cond { expr } else_stmt
  else { expr }
*)
and else_stmt =
  | Else_if of expression * t list * else_stmt
  | Else of t list
  | Nope (* because None is taken *)

(**
  `tag`s can precede all decls, even Curry decl:
  ```
  #[first_tag]
  #[other_tag]
  let add :: a, b = { a + b }
  ```

  Decl:
    let ident :: params -> typ = body
    let ident :: params        = body
    let ident :: ()     -> typ = body
    let ident :: ()            = body

  Curry:
    let ident :: ident <- expression

  Import:
    let ident :: import <- string, string
*)
and decl_stmt =
  | Decl of
      { tags : tag list
      ; name : ident
      ; params : decl_param list
      ; explicit_ret : typ option
      ; body : decl_body
      }
  | Curry_decl of
      { tags : tag list
      ; name : ident
      ; curried : ident
      ; input : expression list
      }
  | Import_decl of
      { name : ident
      ; calling_conf : string
      ; link_name : string
      }
  | Module_decl of
      { name : ident
      ; exports : export_stmt list
      ; body : t list
      }
  | Export_stmt of export_stmt

(**
  export foo
  export { foo, bar as baz }
*)
and export_stmt =
  | Export_ident of ident
  | Export_rename of ident * ident

(**
  statement; statement; ...; expression
*)
and decl_body = statement list * expression option

(**
  ident
  ident : typ
  ?ident : typ = expression
  ?ident = expression
*)
and decl_param =
  | Untyped of ident
  | Typed of ident * typ
  | OptionalTyped of ident * typ * expression
  | OptionalUntyped of ident * expression
  | Variadic of ident

(**
  #[ident]
  #[ident(call_param, ...)]
*)
and tag =
  | Tag_name of ident
  | Tag_call of call_expr

(* EXPRESSION *)
and expression =
  | Call_expr of call_expr
  | Relational_expr of relational_expr
  | Match_expr of match_expr
  | Struct_expr of struct_field list * with_block option
  | Enum_expr of enum_variant list * with_block option
  | Macro_expr of t list
  | Derive_expr of t list

and with_block = t list
and struct_field = ident * typ * expression option
and enum_variant = ident * variant_body option

and variant_body =
  | Struct_body of struct_field list
  | Type_body of typ

and relational_expr =
  | Eql of additive_expr * additive_expr
  | Neq of additive_expr * additive_expr
  | Lt of additive_expr * additive_expr
  | Gt of additive_expr * additive_expr
  | Leq of additive_expr * additive_expr
  | Geq of additive_expr * additive_expr
  | Relational_val of additive_expr

and additive_expr =
  | Add of additive_expr * multiplicative_expr
  | Sub of additive_expr * multiplicative_expr
  | Additive_val of multiplicative_expr

and multiplicative_expr =
  | Mul of multiplicative_expr * unary_expr
  | Div of multiplicative_expr * unary_expr
  | Mod of multiplicative_expr * unary_expr
  | Pow of multiplicative_expr * unary_expr
  | Multiplicative_val of unary_expr

and unary_expr =
  | Neg of unary_expr
  | Not of unary_expr
  | Unary_member of unary_expr * ident
  | Unary_call of call_expr
  | Unary_val of atom

(**
  expression(call_param, ...)
  expression!(call_param, ...)
*)
and call_expr =
  | Decl_call of expression * call_param list
  | Macro_call of expression * call_param list

(**
  ~x = expression
  expression
*)
and call_param =
  | Named of ident * expression
  | Positional of expression

(**
  match expression with
*)
and match_expr = expression * match_arm list

(**
  match_param match_if -> match_arm_body
  match_param -> match_arm_body
*)
and match_arm = match_param * match_if option * match_arm_body

and match_arm_body = statement list * expression option

(**
  Single:
    x
  Touple:
    (x, y, z, ...)
    x, y, z, ...
  Item:
    x :: y :: z :: ...
*)
and match_param =
  | Single of expression
  | Touple of expression list
  | Item of expression list

(** if expression *)
and match_if = expression

and atom =
  | String of string
  | Bool of bool
  | Char of char
  | Int of int
  | Ident of ident
  | Implicit_member of ident
  | Grouping of expression
  | Unit_val

(* PRIMITIVES *)
and ident = string

and typ =
  | User of ident
  | Builtin of ident
  | Unit_typ
[@@deriving show]
