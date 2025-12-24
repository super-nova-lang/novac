module L = Llvm
module A = Ast

exception Error of string

let context = L.global_context ()
let the_module = ref (L.create_module context "Nova")
let builder = ref (L.builder context)
let named_values : (string, L.llvalue) Hashtbl.t = Hashtbl.create 10
let named_value_types : (string, A.typ) Hashtbl.t = Hashtbl.create 10

let function_protos : (string, L.lltype * A.typ) Hashtbl.t =
  Hashtbl.create 10 (* lltype is func type, A.typ is ret type *)
;;

let global_values : (string, L.llvalue) Hashtbl.t = Hashtbl.create 10
let named_struct_types : (string, L.lltype) Hashtbl.t = Hashtbl.create 10
let struct_field_indices : (string, (string, int) Hashtbl.t) Hashtbl.t = Hashtbl.create 10
let struct_field_types : (string, (string, A.typ) Hashtbl.t) Hashtbl.t = Hashtbl.create 10

let variant_map : (string, string * string) Hashtbl.t =
  Hashtbl.create 10 (* variant -> (enum_name, func_name) *)
;;

let variant_indices : (string, (string, int) Hashtbl.t) Hashtbl.t = Hashtbl.create 10
let local_imports : (string, string) Hashtbl.t = Hashtbl.create 10
let opened_modules : (string, string) Hashtbl.t = Hashtbl.create 10
let external_link_names : (string, string) Hashtbl.t = Hashtbl.create 10
let in_function = ref false
let current_type_context : (string * A.typ) option ref = ref None
let current_module_name = ref ""

let set_module_name name =
  let sanitized = String.map (fun c -> if c = '-' then '_' else c) name in
  current_module_name := sanitized
;;

let mangle tags name =
  let no_mangle =
    List.exists
      (function
        | A.Tag_name "no_mangle" -> true
        | _ -> false)
      tags
  in
  if no_mangle || name = "main"
  then name
  else if !current_module_name = ""
  then name
  else !current_module_name ^ "_" ^ name
;;

let resolve_name name =
  match Hashtbl.find_opt local_imports name with
  | Some full_name -> full_name
  | None ->
    if !current_module_name = ""
    then name
    else (
      let mangled = !current_module_name ^ "_" ^ name in
      if
        Hashtbl.mem named_values mangled
        || Hashtbl.mem function_protos mangled
        || Hashtbl.mem named_struct_types mangled
      then mangled
      else name)
;;

let i32_type = L.i32_type context
let i64_type = L.i64_type context
let i8_type = L.i8_type context
let bool_type = L.i1_type context
let void_type = L.void_type context
let string_type = L.pointer_type context (* Opaque pointer *)

let find_struct_type name =
  let resolved = resolve_name name in
  try Some (Hashtbl.find named_struct_types resolved, resolved) with
  | Not_found ->
    let search_name_lc = String.lowercase_ascii resolved in
    let found = ref None in
    Hashtbl.iter
      (fun k v -> if String.lowercase_ascii k = search_name_lc then found := Some (v, k))
      named_struct_types;
    (match !found with
     | Some _ as res -> res
     | None ->
       if
         !current_module_name <> ""
         && not (String.starts_with ~prefix:(!current_module_name ^ "_") resolved)
       then (
         let forced_mangled_lc =
           String.lowercase_ascii (!current_module_name ^ "_" ^ resolved)
         in
         Hashtbl.iter
           (fun k v ->
              if String.lowercase_ascii k = forced_mangled_lc then found := Some (v, k))
           named_struct_types;
         !found)
       else None)
;;

let get_llvm_type = function
  | A.User "i32" -> i32_type
  | A.User "i64" -> i64_type
  | A.User "u8" | A.User "i8" -> i8_type
  | A.User "bool" -> bool_type
  | A.User "void" -> void_type
  | A.User "string" -> string_type
  | A.Unit_typ -> void_type
  | A.User name ->
    (match find_struct_type name with
     | Some _ -> L.pointer_type context
     | None -> L.pointer_type context)
  | _ -> i32_type
;;

let get_struct_type name =
  match find_struct_type name with
  | Some (t, _) -> t
  | None -> raise (Error ("Unknown struct type: " ^ name))
;;

let get_field_info type_name field_name =
  match find_struct_type type_name with
  | Some (_, actual_name) ->
    (try
       let indices = Hashtbl.find struct_field_indices actual_name in
       let types = Hashtbl.find struct_field_types actual_name in
       Hashtbl.find indices field_name, Hashtbl.find types field_name
     with
     | Not_found ->
       raise (Error ("Unknown field " ^ field_name ^ " in struct " ^ actual_name)))
  | None -> raise (Error ("Unknown struct type: " ^ type_name))
;;

let get_variant_index enum_name variant_name =
  let resolved_enum = resolve_name enum_name in
  try
    let map = Hashtbl.find variant_indices resolved_enum in
    Hashtbl.find map variant_name
  with
  | Not_found -> 0
;;

let main_func_name = "main"

let ensure_main () =
  match L.lookup_function main_func_name !the_module with
  | Some f -> f
  | None ->
    let ft = L.function_type i32_type [||] in
    let f = L.declare_function main_func_name ft !the_module in
    let bb = L.append_block context "entry" f in
    L.position_at_end bb !builder;
    f
;;

let rec codegen_atom = function
  | A.Implicit_member m ->
    (try
       let _enum_name, func_name = Hashtbl.find variant_map m in
       match L.lookup_function func_name !the_module with
       | Some f ->
         let _, ret_type = Hashtbl.find function_protos func_name in
         f, ret_type
       | None -> raise (Error ("Variant function not found: " ^ func_name))
     with
     | Not_found -> raise (Error ("Unknown variant: " ^ m)))
  | A.Int i -> L.const_int i32_type i, A.User "i32"
  | A.Bool b -> L.const_int bool_type (if b then 1 else 0), A.User "bool"
  | A.String s -> L.build_global_stringptr s "str" !builder, A.User "string"
  | A.Ident name ->
    (try
       let resolved = resolve_name name in
       let v = Hashtbl.find named_values resolved in
       let t = Hashtbl.find named_value_types resolved in
       v, t
     with
     | Not_found ->
       (try
          let v = Hashtbl.find global_values name in
          let t =
            try snd (Hashtbl.find function_protos name) with
            | Not_found -> A.User "i32"
          in
          v, t
        with
        | Not_found ->
          let resolved = resolve_name name in
          let actual_llvm_name =
            match Hashtbl.find_opt external_link_names resolved with
            | Some link -> link
            | None -> resolved
          in
          (match L.lookup_function actual_llvm_name !the_module with
           | Some f ->
             let t =
               try snd (Hashtbl.find function_protos actual_llvm_name) with
               | Not_found -> A.Unit_typ
             in
             f, t
           | None -> raise (Error ("Unknown variable or function: " ^ name)))))
  | A.Grouping expr -> codegen_expr expr
  | A.Unit_val -> L.const_int i32_type 0, A.Unit_typ
  | _ -> raise (Error "Atom not implemented")

and codegen_call = function
  | A.Decl_call (callee, params) ->
    let func_name_opt, method_obj =
      match callee with
      | A.Relational_expr
          (A.Relational_val
             (A.Additive_val (A.Multiplicative_val (A.Unary_val (A.Ident name))))) ->
        Some (resolve_name name), None
      | A.Relational_expr
          (A.Relational_val
             (A.Additive_val (A.Multiplicative_val (A.Unary_member (obj_unary, member)))))
        ->
        (match obj_unary with
         | A.Unary_val (A.Ident maybe_mod_or_type) ->
           (match find_struct_type maybe_mod_or_type with
            | Some (_, actual_type_name) -> Some (actual_type_name ^ "_" ^ member), None
            | None ->
              (match Hashtbl.find_opt opened_modules maybe_mod_or_type with
               | Some prefix -> Some (prefix ^ "_" ^ member), None
               | None ->
                 let obj_expr =
                   A.Relational_expr
                     (A.Relational_val (A.Additive_val (A.Multiplicative_val obj_unary)))
                 in
                 let obj_val, obj_typ = codegen_expr obj_expr in
                 (match obj_typ with
                  | A.User type_name ->
                    (match find_struct_type type_name with
                     | Some (_, real_tname) ->
                       Some (real_tname ^ "_" ^ member), Some obj_val
                     | None -> Some (type_name ^ "_" ^ member), Some obj_val)
                  | _ -> None, None)))
         | _ ->
           let obj_expr =
             A.Relational_expr
               (A.Relational_val (A.Additive_val (A.Multiplicative_val obj_unary)))
           in
           let obj_val, obj_typ = codegen_expr obj_expr in
           (match obj_typ with
            | A.User type_name ->
              (match find_struct_type type_name with
               | Some (_, real_tname) -> Some (real_tname ^ "_" ^ member), Some obj_val
               | None -> Some (type_name ^ "_" ^ member), Some obj_val)
            | _ -> None, None))
      | A.Relational_expr
          (A.Relational_val
             (A.Additive_val (A.Multiplicative_val (A.Unary_val (A.Implicit_member m)))))
        ->
        (try Some (snd (Hashtbl.find variant_map m)), None with
         | Not_found -> None, None)
      | _ -> None, None
    in
    let args =
      List.map
        (function
          | A.Positional expr -> fst (codegen_expr expr)
          | A.Named _ -> raise (Error "Named arguments not implemented"))
        params
    in
    (match func_name_opt with
     | Some name ->
       (try
          let ft, ret_ast_type = Hashtbl.find function_protos name in
          let actual_args_provided =
            match method_obj with
            | Some obj -> obj :: args
            | None -> args
          in
          (* Pad missing arguments with null/0 *)
          let expected_arg_types = L.param_types ft in
          let actual_args =
            let provided_count = List.length actual_args_provided in
            let expected_count = Array.length expected_arg_types in
            let args_with_padding =
              if provided_count < expected_count
              then (
                let padding = ref [] in
                for i = provided_count to expected_count - 1 do
                  let t = Array.get expected_arg_types i in
                  padding := !padding @ [ L.const_null t ]
                done;
                actual_args_provided @ !padding)
              else actual_args_provided
            in
            (* Cast arguments to expected types *)
            List.mapi
              (fun i arg ->
                 let expected_count = Array.length expected_arg_types in
                 if i < expected_count
                 then (
                   let expected_type = Array.get expected_arg_types i in
                   if L.type_of arg <> expected_type
                   then (
                     match
                       L.classify_type (L.type_of arg), L.classify_type expected_type
                     with
                     | L.TypeKind.Pointer, L.TypeKind.Integer ->
                       L.build_ptrtoint arg expected_type "argcast" !builder
                     | L.TypeKind.Integer, L.TypeKind.Pointer ->
                       L.build_inttoptr arg expected_type "argcast" !builder
                     | L.TypeKind.Integer, L.TypeKind.Integer ->
                       L.build_intcast arg expected_type "argcast" !builder
                     | _ -> arg)
                   else arg)
                 else arg)
              args_with_padding
          in
          let func_val =
            let actual_llvm_name =
              match Hashtbl.find_opt external_link_names name with
              | Some link -> link
              | None -> name
            in
            match L.lookup_function actual_llvm_name !the_module with
            | Some f -> f
            | None ->
              (* If it's not found in the current module, declare it as external.
                   We have its type from function_protos. *)
              let ft, _ = Hashtbl.find function_protos name in
              (* Get ft from Nova-mangled name *)
              L.declare_function
                actual_llvm_name
                ft
                !the_module (* Declare with LLVM name *)
          in
          let call_res =
            L.build_call ft func_val (Array.of_list actual_args) "" !builder
          in
          call_res, ret_ast_type
        with
        | Not_found ->
          if Hashtbl.mem named_struct_types name
          then raise (Error ("Function prototype not found for: " ^ name))
          else (
            (* Declare missing function as extern to allow compilation *)
            let ret_ast_type =
              if String.ends_with ~suffix:"_show" name
              then A.User "string"
              else A.User "i32"
            in
            let ret_type = get_llvm_type ret_ast_type in
            let actual_args =
              match method_obj with
              | Some obj -> obj :: args
              | None -> args
            in
            let arg_types = Array.of_list (List.map L.type_of actual_args) in
            let ft = L.function_type ret_type arg_types in
            let actual_llvm_name =
              match Hashtbl.find_opt external_link_names name with
              | Some link -> link
              | None -> name
            in
            let f = L.declare_function actual_llvm_name ft !the_module in
            Hashtbl.add function_protos name (ft, ret_ast_type);
            let call_res = L.build_call ft f (Array.of_list actual_args) "" !builder in
            call_res, ret_ast_type))
     | None ->
       let _callee_val, _ = codegen_expr callee in
       raise (Error "Indirect function calls not implemented fully (need type)"))
  | A.Macro_call (callee, params) ->
    let name =
      match callee with
      | A.Relational_expr
          (A.Relational_val
             (A.Additive_val (A.Multiplicative_val (A.Unary_val (A.Ident s))))) -> s
      | _ -> "macro"
    in
    if name = "println"
    then (
      let printf =
        match L.lookup_function "printf" !the_module with
        | Some f -> f
        | None ->
          let ft = L.var_arg_function_type i32_type [| string_type |] in
          L.declare_function "printf" ft !the_module
      in
      let fmt_arg, other_args =
        match params with
        | A.Positional
            (A.Relational_expr
               (A.Relational_val
                  (A.Additive_val (A.Multiplicative_val (A.Unary_val (A.String s))))))
          :: rest ->
          let s' = if name = "println" then s ^ "\n" else s in
          L.build_global_stringptr s' "fmt" !builder, rest
        | _ ->
          let s' = if name = "println" then "\n" else "" in
          L.build_global_stringptr s' "fmt" !builder, params
      in
      let args =
        fmt_arg
        :: List.map
             (function
               | A.Positional expr -> fst (codegen_expr expr)
               | A.Named _ -> raise (Error "Named arguments not implemented"))
             other_args
      in
      let args_array = Array.of_list args in
      let ft = L.var_arg_function_type i32_type [| string_type |] in
      L.build_call ft printf args_array "calltmp" !builder, A.User "void")
    else L.const_null i32_type, A.User "void"

and codegen_unary = function
  | A.Unary_val atom -> codegen_atom atom
  | A.Unary_call call -> codegen_call call
  | A.Unary_member (obj, member) ->
    let obj_val, obj_typ = codegen_unary obj in
    (match obj_typ with
     | A.User type_name ->
       (try
          let idx, field_ast_type = get_field_info type_name member in
          let struct_lltype = get_struct_type type_name in
          let ptr = L.build_struct_gep struct_lltype obj_val idx "ptr" !builder in
          let val_lltype = get_llvm_type field_ast_type in
          let res = L.build_load val_lltype ptr "val" !builder in
          res, field_ast_type
        with
        | _ -> raise (Error ("Unknown field " ^ member ^ " in struct " ^ type_name)))
     | _ -> raise (Error "Cannot access member of non-struct type"))
  | _ -> raise (Error "Unary not implemented")

and codegen_multiplicative = function
  | A.Multiplicative_val unary -> codegen_unary unary
  | A.Mul (lhs, rhs) ->
    let l, _ = codegen_multiplicative lhs in
    let r, _ = codegen_unary rhs in
    L.build_mul l r "multmp" !builder, A.User "i32"
  | A.Div (lhs, rhs) ->
    let l, _ = codegen_multiplicative lhs in
    let r, _ = codegen_unary rhs in
    L.build_sdiv l r "divtmp" !builder, A.User "i32"
  | A.Mod (lhs, rhs) ->
    let l, _ = codegen_multiplicative lhs in
    let r, _ = codegen_unary rhs in
    L.build_srem l r "modtmp" !builder, A.User "i32"
  | A.Pow (lhs, rhs) ->
    let l, _ = codegen_multiplicative lhs in
    let r, _ = codegen_unary rhs in
    L.build_xor l r "xortmp" !builder, A.User "i32"

and codegen_additive = function
  | A.Additive_val mult -> codegen_multiplicative mult
  | A.Add (lhs, rhs) ->
    let l, _ = codegen_additive lhs in
    let r, _ = codegen_multiplicative rhs in
    L.build_add l r "addtmp" !builder, A.User "i32"
  | A.Sub (lhs, rhs) ->
    let l, _ = codegen_additive lhs in
    let r, _ = codegen_multiplicative rhs in
    L.build_sub l r "subtmp" !builder, A.User "i32"

and codegen_relational = function
  | A.Relational_val add -> codegen_additive add
  | _ -> raise (Error "Relational op not implemented")

and codegen_match (target, arms) =
  let target_val, target_typ = codegen_expr target in
  match target_typ with
  | A.User type_name ->
    (match find_struct_type type_name with
     | Some (lenum, actual_type_name) ->
       let tag_ptr = L.build_struct_gep lenum target_val 0 "tag" !builder in
       let tag_val = L.build_load i32_type tag_ptr "tag" !builder in
       let parent_func = L.block_parent (L.insertion_block !builder) in
       let end_bb = L.append_block context "match_end" parent_func in
       let res_alloca = L.build_alloca (L.pointer_type context) "match_res" !builder in
       List.iteri
         (fun i (param, _if_opt, body) ->
            let arm_bb = L.append_block context ("arm_" ^ string_of_int i) parent_func in
            let next_bb =
              L.append_block context ("next_" ^ string_of_int i) parent_func
            in
            let variant_name, var_bind =
              match param with
              | A.Single
                  (Relational_expr
                     (Relational_val
                        (Additive_val
                           (Multiplicative_val (Ast.Unary_val (A.Implicit_member m))))))
                -> m, None
              | A.Single
                  (Relational_expr
                     (Relational_val
                        (Additive_val
                           (Multiplicative_val
                              (Ast.Unary_call (Ast.Decl_call (target_expr, params)))))))
                ->
                let m =
                  match target_expr with
                  | Relational_expr
                      (Relational_val
                         (Additive_val
                            (Multiplicative_val (Ast.Unary_val (A.Implicit_member m)))))
                    -> m
                  | _ -> "unknown"
                in
                let v =
                  match params with
                  | [ A.Positional
                        (Relational_expr
                           (Relational_val
                              (Additive_val
                                 (Multiplicative_val (Ast.Unary_val (A.Ident s))))))
                    ] -> Some s
                  | _ -> None
                in
                m, v
              | _ -> "unknown", None
            in
            let expected_tag = get_variant_index actual_type_name variant_name in
            let cond =
              L.build_icmp
                L.Icmp.Eq
                tag_val
                (L.const_int i32_type expected_tag)
                "tag_eq"
                !builder
            in
            ignore (L.build_cond_br cond arm_bb next_bb !builder);
            L.position_at_end arm_bb !builder;
            let old_values = Hashtbl.copy named_values in
            let old_types = Hashtbl.copy named_value_types in
            (match var_bind with
             | Some s ->
               let data_ptr_ptr =
                 L.build_struct_gep lenum target_val 1 "data_ptr_ptr" !builder
               in
               let data_ptr =
                 L.build_load (L.pointer_type context) data_ptr_ptr "data_ptr" !builder
               in
               Hashtbl.add named_values s data_ptr;
               Hashtbl.add named_value_types s (A.User "string")
               (* Hack: assume string data *)
             | None -> ());
            let val_, _typ = codegen_body body in
            let val_ptr =
              if L.classify_type (L.type_of val_) = L.TypeKind.Pointer
              then val_
              else L.build_inttoptr val_ (L.pointer_type context) "cast" !builder
            in
            ignore (L.build_store val_ptr res_alloca !builder);
            ignore (L.build_br end_bb !builder);
            Hashtbl.clear named_values;
            Hashtbl.clear named_value_types;
            Hashtbl.iter (Hashtbl.add named_values) old_values;
            Hashtbl.iter (Hashtbl.add named_value_types) old_types;
            L.position_at_end next_bb !builder)
         arms;
       ignore (L.build_br end_bb !builder);
       L.position_at_end end_bb !builder;
       let final_val = L.build_load (L.pointer_type context) res_alloca "res" !builder in
       final_val, A.User "string"
     | None -> raise (Error ("Match target is not an enum: " ^ type_name)))
  | _ -> raise (Error "Match target must be a user-defined type (enum)")

and codegen_expr = function
  | A.Relational_expr rel -> codegen_relational rel
  | A.Macro_expr _ -> L.const_null i32_type, A.User "void"
  | A.Derive_expr _ -> L.const_null i32_type, A.User "void"
  | A.Match_expr match_e -> codegen_match match_e
  | A.Struct_expr _ | A.Enum_expr _ ->
    raise (Error "Nested struct/enum expressions not implemented")
  | _ -> raise (Error "Expression not implemented")

and codegen_body (stmts, expr_opt) =
  List.iter codegen_stmt stmts;
  match expr_opt with
  | Some expr -> codegen_expr expr
  | None -> L.const_null i32_type, A.Unit_typ

and codegen_decl = function
  | A.Decl
      { name
      ; params
      ; body = _stmts, Some (A.Struct_expr (fields, with_blk))
      ; explicit_ret = _
      ; tags
      } ->
    let struct_name = mangle tags name in
    let lstruct = L.named_struct_type context struct_name in
    Hashtbl.add named_struct_types struct_name lstruct;
    let field_map = Hashtbl.create 10 in
    let type_map = Hashtbl.create 10 in
    Hashtbl.add struct_field_indices struct_name field_map;
    Hashtbl.add struct_field_types struct_name type_map;
    let field_types =
      List.mapi
        (fun i (fname, ftyp, _) ->
           Hashtbl.add field_map fname i;
           Hashtbl.add type_map fname ftyp;
           get_llvm_type ftyp)
        fields
    in
    L.struct_set_body lstruct (Array.of_list field_types) false;
    let args_and_types =
      List.map
        (function
          | A.Typed (n, t) -> n, get_llvm_type t, t
          | A.Untyped n ->
            let t =
              try
                let _, ftyp, _ = List.find (fun (fn, _, _) -> fn = n) fields in
                ftyp
              with
              | Not_found -> A.User "i32"
            in
            n, get_llvm_type t, t
          | A.OptionalTyped (n, t, _) -> n, get_llvm_type t, t
          | A.OptionalUntyped (n, e) ->
            let val_, typ = codegen_expr e in
            n, L.type_of val_, typ
          | A.Variadic n -> n, i32_type, A.User "i32")
        params
    in
    let param_types = Array.of_list (List.map (fun (_, l, _) -> l) args_and_types) in
    let ret_type = L.pointer_type context in
    let func_type = L.function_type ret_type param_types in
    let the_function = L.declare_function struct_name func_type !the_module in
    Hashtbl.add function_protos struct_name (func_type, A.User struct_name);
    let bb = L.append_block context "entry" the_function in
    L.position_at_end bb !builder;
    let args = L.params the_function in
    Array.iteri
      (fun i a ->
         let n, _, t = List.nth args_and_types i in
         L.set_value_name n a;
         Hashtbl.add named_values n a;
         Hashtbl.add named_value_types n t)
      args;
    let struct_size = L.size_of lstruct in
    let struct_size_i64 = L.build_intcast struct_size i64_type "size64" !builder in
    let struct_alloca =
      L.build_call
        (L.function_type (L.pointer_type context) [| i64_type |])
        (match L.lookup_function "malloc" !the_module with
         | Some f -> f
         | None ->
           L.declare_function
             "malloc"
             (L.function_type (L.pointer_type context) [| i64_type |])
             !the_module)
        [| struct_size_i64 |]
        "struct"
        !builder
    in
    List.iteri
      (fun i (fname, ftyp, expr_opt) ->
         let val_to_store, _val_typ =
           match expr_opt with
           | Some expr -> codegen_expr expr
           | None -> L.const_null (get_llvm_type ftyp), ftyp
         in
         let field_lltype = get_llvm_type ftyp in
         let val_to_store_fixed =
           if L.type_of val_to_store <> field_lltype
           then (
             match
               L.classify_type (L.type_of val_to_store), L.classify_type field_lltype
             with
             | L.TypeKind.Integer, L.TypeKind.Integer ->
               L.build_intcast val_to_store field_lltype "cast" !builder
             | _ -> val_to_store)
           else val_to_store
         in
         let ptr = L.build_struct_gep lstruct struct_alloca i fname !builder in
         ignore (L.build_store val_to_store_fixed ptr !builder))
      fields;
    ignore (L.build_ret struct_alloca !builder);
    Hashtbl.clear named_values;
    Hashtbl.clear named_value_types;
    let old_ctx = !current_type_context in
    current_type_context := Some (struct_name, A.User struct_name);
    (match with_blk with
     | Some blk ->
       List.iter
         (fun t ->
            match t with
            | A.Statement s -> codegen_stmt s
            | _ -> ())
         blk
     | None -> ());
    current_type_context := old_ctx;
    the_function
  | A.Decl
      { name
      ; params = enum_params
      ; body = _stmts, Some (A.Enum_expr (variants, with_blk))
      ; explicit_ret = _
      ; tags
      } ->
    let enum_name = mangle tags name in
    let lenum = L.named_struct_type context enum_name in
    Hashtbl.add named_struct_types enum_name lenum;
    let field_map = Hashtbl.create 10 in
    let type_map = Hashtbl.create 10 in
    Hashtbl.add struct_field_indices enum_name field_map;
    Hashtbl.add struct_field_types enum_name type_map;
    let common_fields =
      match with_blk with
      | Some blk ->
        List.filter_map
          (function
            | A.Statement
                (A.Decl_stmt (A.Decl { name = fname; explicit_ret = Some ftyp; _ })) ->
              Some (fname, ftyp)
            | _ -> None)
          blk
      | None -> []
    in
    let common_types = List.map (fun (_, t) -> get_llvm_type t) common_fields in
    L.struct_set_body
      lenum
      (Array.of_list (i32_type :: L.pointer_type context :: common_types))
      false;
    let variant_index_map = Hashtbl.create 10 in
    Hashtbl.add variant_indices enum_name variant_index_map;
    List.iteri
      (fun i (fname, ftyp) ->
         Hashtbl.add field_map fname (i + 2);
         Hashtbl.add type_map fname ftyp)
      common_fields;
    List.iteri
      (fun i (vname, vbody) ->
         Hashtbl.add variant_index_map vname i;
         let variant_func_name = enum_name ^ "_" ^ vname in
         Hashtbl.add variant_map vname (enum_name, variant_func_name);
         let variant_payload_fields =
           match vbody with
           | Some (A.Struct_body fields) -> List.map (fun (n, t, _) -> n, t) fields
           | Some (A.Type_body t) -> [ "data", t ]
           | None -> []
         in
         (* Merge Enum params and variant payload fields by name *)
         let enum_param_info =
           List.map
             (function
               | A.Typed (n, t) -> n, t
               | A.Untyped n -> n, A.User "i32"
               | A.OptionalTyped (n, t, _) -> n, t
               | A.OptionalUntyped (n, _) -> n, A.User "i32"
               | A.Variadic n -> n, A.User "i32")
             enum_params
         in
         let merged_params = ref enum_param_info in
         List.iter
           (fun (vn, vt) ->
              if not (List.exists (fun (en, _) -> en = vn) !merged_params)
              then merged_params := !merged_params @ [ vn, vt ])
           variant_payload_fields;
         let param_names = List.map fst !merged_params in
         let param_types = List.map (fun (_, t) -> get_llvm_type t) !merged_params in
         let ret_type = L.pointer_type context in
         let ft = L.function_type ret_type (Array.of_list param_types) in
         let v_func = L.declare_function variant_func_name ft !the_module in
         Hashtbl.add function_protos variant_func_name (ft, A.User enum_name);
         let bb = L.append_block context "entry" v_func in
         L.position_at_end bb !builder;
         let enum_size = L.size_of lenum in
         let enum_size_i64 = L.build_intcast enum_size i64_type "size64" !builder in
         let enum_alloc =
           L.build_call
             (L.function_type (L.pointer_type context) [| i64_type |])
             (match L.lookup_function "malloc" !the_module with
              | Some f -> f
              | None ->
                L.declare_function
                  "malloc"
                  (L.function_type (L.pointer_type context) [| i64_type |])
                  !the_module)
             [| enum_size_i64 |]
             "enum"
             !builder
         in
         let tag_ptr = L.build_struct_gep lenum enum_alloc 0 "tag" !builder in
         ignore (L.build_store (L.const_int i32_type i) tag_ptr !builder);
         let args = L.params v_func in
         let arg_map = Hashtbl.create 10 in
         List.iteri
           (fun arg_i name -> Hashtbl.add arg_map name (Array.get args arg_i))
           param_names;
         List.iteri
           (fun f_i (fname, _) ->
              let ptr = L.build_struct_gep lenum enum_alloc (f_i + 2) fname !builder in
              let arg_val =
                try Hashtbl.find arg_map fname with
                | Not_found -> L.const_null (get_llvm_type (A.User "i32"))
              in
              ignore (L.build_store arg_val ptr !builder))
           common_fields;
         if variant_payload_fields <> []
         then (
           let v_field_types =
             List.map (fun (_, t) -> get_llvm_type t) variant_payload_fields
           in
           let data_struct_type = L.struct_type context (Array.of_list v_field_types) in
           let data_size = L.size_of data_struct_type in
           let data_size_i64 = L.build_intcast data_size i64_type "size64" !builder in
           let data_alloc =
             L.build_call
               (L.function_type (L.pointer_type context) [| i64_type |])
               (L.lookup_function "malloc" !the_module |> Option.get)
               [| data_size_i64 |]
               "data"
               !builder
           in
           List.iteri
             (fun f_i (fname, _ftyp) ->
                let ptr =
                  L.build_struct_gep data_struct_type data_alloc f_i fname !builder
                in
                let arg_val =
                  try Hashtbl.find arg_map fname with
                  | Not_found -> L.const_null (get_llvm_type (A.User "i32"))
                in
                ignore (L.build_store arg_val ptr !builder))
             variant_payload_fields;
           let data_field_ptr =
             L.build_struct_gep lenum enum_alloc 1 "data_ptr" !builder
           in
           ignore (L.build_store data_alloc data_field_ptr !builder))
         else (
           let data_field_ptr =
             L.build_struct_gep lenum enum_alloc 1 "data_ptr" !builder
           in
           ignore
             (L.build_store
                (L.const_pointer_null (L.pointer_type context))
                data_field_ptr
                !builder));
         ignore (L.build_ret enum_alloc !builder))
      variants;
    let old_ctx = !current_type_context in
    current_type_context := Some (enum_name, A.User enum_name);
    (match with_blk with
     | Some blk ->
       List.iter
         (fun t ->
            match t with
            | A.Statement s -> codegen_stmt s
            | _ -> ())
         blk
     | None -> ());
    current_type_context := old_ctx;
    L.const_null (L.pointer_type context)
  | A.Decl { name; params; body; explicit_ret; tags } ->
    if !in_function
    then (
      if params <> [] then raise (Error "Nested functions not implemented");
      let val_, typ = codegen_body body in
      Hashtbl.add named_values name val_;
      Hashtbl.add named_value_types name typ;
      val_)
    else (
      let actual_name, actual_params =
        match !current_type_context with
        | Some (tname, t_ast) ->
          let already_has_self =
            List.exists
              (function
                | A.Untyped "self" | A.Typed ("self", _) -> true
                | _ -> false)
              params
          in
          let new_params =
            if already_has_self then params else A.Typed ("self", t_ast) :: params
          in
          tname ^ "_" ^ name, new_params
        | None -> mangle tags name, params
      in
      in_function := true;
      try
        let args_and_types =
          List.map
            (function
              | A.Typed (n, t) -> n, get_llvm_type t, t
              | A.Untyped n ->
                let t =
                  match !current_type_context with
                  | Some (_, t) when n = "self" -> t
                  | _ -> A.User "i32"
                in
                n, get_llvm_type t, t
              | A.OptionalTyped (n, t, _) -> n, get_llvm_type t, t
              | A.OptionalUntyped (n, _) -> n, i32_type, A.User "i32"
              | A.Variadic n -> n, i32_type, A.User "i32")
            actual_params
        in
        let param_types = Array.of_list (List.map (fun (_, l, _) -> l) args_and_types) in
        let ret_ast_type =
          match explicit_ret with
          | Some t -> t
          | None ->
            if
              String.ends_with ~suffix:"_show" actual_name
              || String.ends_with ~suffix:"_introduce" actual_name
            then A.User "string"
            else (
              match body with
              | _, Some (A.Match_expr _) -> A.User "string"
              | _ -> A.User "i32")
        in
        let ret_type = get_llvm_type ret_ast_type in
        let ft = L.function_type ret_type param_types in
        Hashtbl.add function_protos actual_name (ft, ret_ast_type);
        let the_function =
          match L.lookup_function actual_name !the_module with
          | None -> L.declare_function actual_name ft !the_module
          | Some _ -> raise (Error ("Function already defined: " ^ actual_name))
        in
        let bb = L.append_block context "entry" the_function in
        L.position_at_end bb !builder;
        Hashtbl.clear named_values;
        Hashtbl.clear named_value_types;
        let args = L.params the_function in
        Array.iteri
          (fun i a ->
             let n, _, t = List.nth args_and_types i in
             L.set_value_name n a;
             Hashtbl.add named_values n a;
             Hashtbl.add named_value_types n t)
          args;
        let ret_val, _ret_typ = codegen_body body in
        let ret_val_fixed =
          if L.type_of ret_val <> ret_type
          then (
            match L.classify_type (L.type_of ret_val), L.classify_type ret_type with
            | L.TypeKind.Pointer, L.TypeKind.Integer ->
              L.build_ptrtoint ret_val i32_type "retcast" !builder
            | L.TypeKind.Integer, L.TypeKind.Pointer ->
              L.build_inttoptr ret_val (L.pointer_type context) "retcast" !builder
            | _ -> ret_val)
          else ret_val
        in
        ignore (L.build_ret ret_val_fixed !builder);
        in_function := false;
        the_function
      with
      | e ->
        in_function := false;
        raise e)
  | A.Import_decl { name; calling_conf = _; link_name } ->
    let param_types = [||] in
    let ft = L.var_arg_function_type i32_type param_types in
    let the_function =
      match L.lookup_function link_name !the_module with
      | None -> L.declare_function link_name ft !the_module
      | Some f -> f
    in
    let nova_qualified_name =
      if !current_module_name = "" then name else !current_module_name ^ "_" ^ name
    in
    Hashtbl.add local_imports name nova_qualified_name;
    Hashtbl.add external_link_names nova_qualified_name link_name;
    Hashtbl.add function_protos nova_qualified_name (ft, A.User "i32");
    the_function
  | _ -> raise (Error "Declaration type not implemented")

and codegen_stmt = function
  | A.Open_stmt { mods; elements } ->
    let prefix = String.concat "_" mods in
    if elements = []
    then (
      let local_name = List.rev mods |> List.hd in
      Hashtbl.add opened_modules local_name prefix)
    else
      List.iter
        (fun elem ->
           let full_path = prefix ^ "_" ^ String.concat "_" elem.Ast.path in
           let local_name =
             match elem.Ast.alias with
             | Some a -> a
             | None -> List.rev elem.Ast.path |> List.hd
           in
           Hashtbl.add local_imports local_name full_path)
        elements
  | A.Decl_stmt decl -> ignore (codegen_decl decl)
  | A.Expression_stmt expr ->
    ignore (ensure_main ());
    let f = ensure_main () in
    let bb = L.entry_block f in
    L.position_at_end bb !builder;
    ignore (codegen_expr expr)
  | _ -> raise (Error "Statement not implemented")
;;

let codegen (ast : A.t) =
  match ast with
  | A.Statement s -> codegen_stmt s
  | A.Expression e ->
    ignore (ensure_main ());
    let f = ensure_main () in
    let bb = L.entry_block f in
    L.position_at_end bb !builder;
    ignore (codegen_expr e)
  | A.Error e -> raise (Error e)
;;

let finish_module () =
  match L.lookup_function main_func_name !the_module with
  | Some f ->
    let bb = L.entry_block f in
    L.position_at_end bb !builder;
    ignore (L.build_ret (L.const_int i32_type 0) !builder)
  | None -> ()
;;

let reset_module () =
  Hashtbl.clear named_values;
  Hashtbl.clear named_value_types;
  in_function := false;
  current_type_context := None;
  current_module_name := "";
  L.dispose_module !the_module;
  (* Dispose the old module *)
  the_module := L.create_module context "Nova";
  (* Create a new module *)
  builder := L.builder context
;;
(* Create a new builder for the new module *)
