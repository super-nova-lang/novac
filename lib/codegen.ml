module L = Llvm
module A = Ast

exception Error of string

let context = L.global_context ()
let the_module = L.create_module context "Nova"
let builder = L.builder context
let named_values : (string, L.llvalue) Hashtbl.t = Hashtbl.create 10
let function_protos : (string, L.lltype) Hashtbl.t = Hashtbl.create 10
let global_values : (string, L.llvalue) Hashtbl.t = Hashtbl.create 10
let in_function = ref false
let i32_type = L.i32_type context
let i64_type = L.i64_type context
let bool_type = L.i1_type context
let void_type = L.void_type context

let rec codegen_atom = function
  | A.Int i -> L.const_int i32_type i
  | A.Bool b -> L.const_int bool_type (if b then 1 else 0)
  | A.String s -> L.build_global_stringptr s "str" builder
  | A.Ident name ->
    (try Hashtbl.find named_values name with
     | Not_found ->
       (try Hashtbl.find global_values name with
        | Not_found ->
          (match L.lookup_function name the_module with
           | Some f -> f
           | None -> raise (Error ("Unknown variable or function: " ^ name)))))
  | A.Grouping expr -> codegen_expr expr
  | _ -> raise (Error "Atom not implemented")

and codegen_call = function
  | A.Decl_call (callee, params) ->
    let callee_val = codegen_expr callee in
    let args =
      List.map
        (function
          | A.Positional expr -> codegen_expr expr
          | A.Named _ -> raise (Error "Named arguments not implemented"))
        params
    in
    let callee_type =
      match callee with
      | A.Relational_expr (A.Relational_val (A.Additive_val (A.Multiplicative_val (A.Unary_val (A.Ident name))))) ->
        (try Hashtbl.find function_protos name with
         | Not_found -> raise (Error ("Function prototype not found for: " ^ name)))
      | _ -> raise (Error "Indirect function calls not implemented (cannot determine type)")
    in
    L.build_call callee_type callee_val (Array.of_list args) "calltmp" builder
  | A.Macro_call _ -> raise (Error "Macro calls not implemented")

and codegen_unary = function
  | A.Unary_val atom -> codegen_atom atom
  | A.Unary_call call -> codegen_call call
  | _ -> raise (Error "Unary not implemented")

and codegen_multiplicative = function
  | A.Multiplicative_val unary -> codegen_unary unary
  | A.Mul (lhs, rhs) ->
    let l = codegen_multiplicative lhs in
    let r = codegen_unary rhs in
    L.build_mul l r "multmp" builder
  | A.Div (lhs, rhs) ->
    let l = codegen_multiplicative lhs in
    let r = codegen_unary rhs in
    L.build_sdiv l r "divtmp" builder
  | A.Mod (lhs, rhs) ->
    let l = codegen_multiplicative lhs in
    let r = codegen_unary rhs in
    L.build_srem l r "modtmp" builder
  | A.Pow (lhs, rhs) ->
    let l = codegen_multiplicative lhs in
    let r = codegen_unary rhs in
    L.build_xor l r "xortmp" builder

and codegen_additive = function
  | A.Additive_val mult -> codegen_multiplicative mult
  | A.Add (lhs, rhs) ->
    let l = codegen_additive lhs in
    let r = codegen_multiplicative rhs in
    L.build_add l r "addtmp" builder
  | A.Sub (lhs, rhs) ->
    let l = codegen_additive lhs in
    let r = codegen_multiplicative rhs in
    L.build_sub l r "subtmp" builder

and codegen_relational = function
  | A.Relational_val add -> codegen_additive add
  | _ -> raise (Error "Relational op not implemented")

and codegen_expr = function
  | A.Relational_expr rel -> codegen_relational rel
  | _ -> raise (Error "Expression not implemented")
;;

let get_llvm_type = function
  | A.User "i32" -> i32_type
  | A.User "bool" -> bool_type
  | A.User "void" -> void_type
  | A.Unit_typ -> void_type
  | _ -> i32_type (* Default to i32 for now *)
;;

let rec codegen_body (stmts, expr_opt) =
  List.iter codegen_stmt stmts;
  match expr_opt with
  | Some expr -> codegen_expr expr
  | None -> L.const_null i32_type (* Default void/nil *)

and codegen_decl = function
  | A.Decl { name; params; body; explicit_ret; _ } ->
    if !in_function
    then (
      (* Local declaration *)
      if params <> [] then raise (Error "Nested functions not implemented");
      let val_ = codegen_body body in
      Hashtbl.add named_values name val_;
      val_)
    else (
      (* Global function declaration *)
      in_function := true;
      try
        (* Prepare arguments *)
        let args_and_types =
          List.map
            (function
              | A.Typed (n, t) -> n, get_llvm_type t
              | A.Untyped n -> n, i32_type (* Default to i32 *)
              | _ -> raise (Error "Param type not supported"))
            params
        in
        let param_types = Array.of_list (List.map snd args_and_types) in
        let ret_type =
          match explicit_ret with
          | Some t -> get_llvm_type t
          | None -> i32_type
        in
        let ft = L.function_type ret_type param_types in
        Hashtbl.add function_protos name ft;
        let the_function =
          match L.lookup_function name the_module with
          | None -> L.declare_function name ft the_module
          | Some _ -> raise (Error ("Function already defined: " ^ name))
        in
        (* Create new block for function body *)
        let bb = L.append_block context "entry" the_function in
        L.position_at_end bb builder;
        (* Update symbol table with args *)
        Hashtbl.clear named_values;
        let args = L.params the_function in
        Array.iteri
          (fun i a ->
             let n, _ = List.nth args_and_types i in
             L.set_value_name n a;
             Hashtbl.add named_values n a)
          args;
        (* Generate body *)
        let ret_val = codegen_body body in
        ignore (L.build_ret ret_val builder);
        in_function := false;
        the_function
      with
      | e ->
        in_function := false;
        raise e)
  | A.Import_decl { name; calling_conf = _; link_name } ->
    let ret_type = i32_type in
    let param_types = [| |] in
    let ft = L.var_arg_function_type ret_type param_types in
    let the_function =
      match L.lookup_function link_name the_module with
      | None -> L.declare_function link_name ft the_module
      | Some f -> f
    in
    Hashtbl.add global_values name the_function;
    Hashtbl.add function_protos name ft;
    the_function
  | _ -> raise (Error "Declaration type not implemented")

and codegen_stmt = function
  | A.Decl_stmt decl -> ignore (codegen_decl decl)
  | A.Expression_stmt expr -> ignore (codegen_expr expr)
  | _ -> raise (Error "Statement not implemented")
;;

let codegen (ast : A.t) =
  match ast with
  | A.Statement s -> codegen_stmt s
  | A.Expression e -> ignore (codegen_expr e)
  | A.Error e -> raise (Error e)
;;
