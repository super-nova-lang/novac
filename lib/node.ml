type t =
  | Type_ident of string
  | TODO_complex

and param = string * t option

and curry_arg =
  | Positional of node list (* using node list as proxy for expr *)
  | Named of string * node list

and node =
  | Open_mod of string
  | Decl of
      { name : string
      ; params : param list
      ; ret_type : t option
      ; body : node list
      }
  | Curry_Decl of
      { name : string
      ; target_fn : string
      ; args : curry_arg list
      }
  | Tag of string
  | Expr of Token.t list (* Placeholder for expression logic *)
  | Unhandled of Token.t
[@@deriving show]
