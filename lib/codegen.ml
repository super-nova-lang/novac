module StringSet = Set.Make (String)
module StringMap = Map.Make (String)

let ( let* ) = Result.bind

let ensure_semantics nodes =
  let errors, _warnings = Analysis.analyze nodes in
  match
    List.find_map
      (function
        | Analysis.Error err -> Some err
        | Analysis.Warning _ -> None)
      errors
  with
  | Some err -> Result.Error err
  | None -> Result.Ok ()
;;

module Target = struct
  let ptr_size = 8
  let exit_syscall = 60
end

module Emitter = struct
  module SymbolSet = Set.Make (String)

  type t =
    { text : Buffer.t
    ; data : Buffer.t
    ; bss : Buffer.t
    ; globals : SymbolSet.t ref
    ; externs : SymbolSet.t ref
    }

  let create () =
    { text = Buffer.create 512
    ; data = Buffer.create 128
    ; bss = Buffer.create 128
    ; globals = ref SymbolSet.empty
    ; externs = ref SymbolSet.empty
    }
  ;;

  let add_global t sym = t.globals := SymbolSet.add sym !(t.globals)
  let add_extern t sym = t.externs := SymbolSet.add sym !(t.externs)

  let emit_line buf line =
    Buffer.add_string buf line;
    Buffer.add_char buf '\n'
  ;;

  let emit_text t line = emit_line t.text line
  let emit_data t line = emit_line t.data line
  let emit_bss t line = emit_line t.bss line

  let render_section name buf out =
    if Buffer.length buf = 0
    then out
    else out ^ "section ." ^ name ^ "\n" ^ Buffer.contents buf
  ;;

  let render t =
    let program = Buffer.create 1024 in
    SymbolSet.iter (fun e -> emit_line program ("extern " ^ e)) !(t.externs);
    SymbolSet.iter (fun g -> emit_line program ("global " ^ g)) !(t.globals);
    let sections =
      ""
      |> render_section "data" t.data
      |> render_section "bss" t.bss
      |> render_section "text" t.text
    in
    Buffer.add_string program sections;
    Buffer.contents program
  ;;
end

let anon_struct_counter = ref 0
let anon_enum_counter = ref 0
let emit_instr emitter text = Emitter.emit_text emitter ("  " ^ text)
let emit_label emitter label = Emitter.emit_text emitter (label ^ ":")

let emit_placeholder emitter msg =
  emit_instr emitter ("; " ^ msg);
  emit_instr emitter "mov rax, 0";
  Result.Ok ()
;;

let unsupported emitter msg = emit_placeholder emitter msg
let arg_registers = [| "rdi"; "rsi"; "rdx"; "rcx"; "r8"; "r9" |]
let string_table : (string, string) Hashtbl.t = Hashtbl.create 32
let string_counter = ref 0
let defined_functions = ref StringSet.empty
let gensym_counter = ref 0

let gensym prefix =
  let n = !gensym_counter in
  incr gensym_counter;
  Printf.sprintf "__%s_%d" prefix n
;;

(* --- Value typing and layouts --- *)

type value_type =
  | Type_int
  | Type_bool
  | Type_char
  | Type_string
  | Type_unit
  | Type_struct of string
  | Type_enum of string
  | Type_unknown

type field_layout =
  { fname : string
  ; foffset : int
  ; ftype : value_type
  }

type struct_layout =
  { s_fields : field_layout list
  ; s_size : int
  }

type variant_layout =
  { vname : string
  ; v_payload : field_layout list
  }

type enum_layout =
  { e_shared : field_layout list
  ; e_variants : variant_layout list
  ; e_payload_size : int (* bytes for payload area *)
  }

let struct_layouts : struct_layout StringMap.t ref = ref StringMap.empty
let enum_layouts : enum_layout StringMap.t ref = ref StringMap.empty

let type_of_typ = function
  | Ast.User "i32" -> Type_int
  | Ast.User "bool" -> Type_bool
  | Ast.User "char" -> Type_char
  | Ast.User "string" -> Type_string
  | Ast.Unit_typ -> Type_unit
  | Ast.Builtin _ -> Type_unknown
  | Ast.User n when StringMap.mem n !struct_layouts -> Type_struct n
  | Ast.User n when StringMap.mem n !enum_layouts -> Type_enum n
  | Ast.User n -> Type_struct n
;;

let field_offset idx = (idx + 1) * Target.ptr_size

let build_struct_layout _name fields =
  let rec fold idx acc = function
    | [] -> List.rev acc
    | (fname, ftyp, _expr_opt) :: rest ->
      let ftype = type_of_typ ftyp in
      let fl = { fname; foffset = field_offset idx; ftype } in
      fold (idx + 1) (fl :: acc) rest
  in
  let s_fields = fold 0 [] fields in
  let s_size = List.length s_fields * Target.ptr_size in
  { s_fields; s_size }
;;

let build_enum_layout _name params variants =
  let shared_fields =
    List.mapi
      (fun idx -> function
         | Ast.Typed (id, t) ->
           { fname = id; foffset = field_offset idx; ftype = type_of_typ t }
         | Ast.Untyped id ->
           { fname = id; foffset = field_offset idx; ftype = Type_unknown }
         | _ -> { fname = ""; foffset = 0; ftype = Type_unknown })
      params
    |> List.filter (fun f -> f.fname <> "")
  in
  let shared_bytes = List.length shared_fields * Target.ptr_size in
  let rec payload_fields_of_struct idx acc = function
    | [] -> List.rev acc
    | (fname, ftyp, _e) :: rest ->
      let ftype = type_of_typ ftyp in
      let fl = { fname; foffset = shared_bytes + field_offset idx; ftype } in
      payload_fields_of_struct (idx + 1) (fl :: acc) rest
  in
  let variant_layouts, max_payload_slots =
    List.fold_left
      (fun (acc, max_slots) (vname, body_opt) ->
         match body_opt with
         | None -> { vname; v_payload = [] } :: acc, max max_slots 0
         | Some (Ast.Struct_body sfields) ->
           let payload = payload_fields_of_struct 0 [] sfields in
           { vname; v_payload = payload } :: acc, max max_slots (List.length sfields)
         | Some (Ast.Type_body _) ->
           let payload =
             [ { fname = "payload"
               ; foffset = shared_bytes + field_offset 0
               ; ftype = Type_unknown
               }
             ]
           in
           { vname; v_payload = payload } :: acc, max max_slots 1)
      ([], 0)
      variants
  in
  let payload_size = max_payload_slots * Target.ptr_size in
  { e_shared = shared_fields
  ; e_variants = List.rev variant_layouts
  ; e_payload_size = payload_size
  }
;;

let collect_layouts nodes =
  let rec visit prefix = function
    | [] -> ()
    | Ast.Statement stmt :: rest ->
      (match stmt with
       | Ast.Decl_stmt (Ast.Decl { name; params; body = _, expr_opt; _ }) ->
         let symbol = String.concat "_" (prefix @ [ name ]) in
         (match expr_opt with
          | Some (Ast.Struct_expr (fields, _)) ->
            let layout = build_struct_layout symbol fields in
            struct_layouts := StringMap.add symbol layout !struct_layouts
          | Some (Ast.Enum_expr (variants, _)) ->
            let layout = build_enum_layout symbol params variants in
            enum_layouts := StringMap.add symbol layout !enum_layouts
          | _ -> ());
         visit prefix rest
       | Ast.Decl_stmt (Ast.Module_decl { name; body; _ }) ->
         visit (prefix @ [ name ]) body;
         visit prefix rest
       | _ -> visit prefix rest)
    | _ :: rest -> visit prefix rest
  in
  struct_layouts := StringMap.empty;
  enum_layouts := StringMap.empty;
  visit [] nodes
;;

let find_struct_layout_by_fields fields =
  let field_names = List.map (fun (n, _, _) -> n) fields in
  StringMap.fold
    (fun name layout acc ->
       match acc with
       | Some _ -> acc
       | None ->
         let layout_names = List.map (fun f -> f.fname) layout.s_fields in
         if layout_names = field_names then Some (name, layout) else None)
    !struct_layouts
    None
;;

let find_enum_layout_by_variants variants =
  let variant_names = List.map fst variants in
  StringMap.fold
    (fun name layout acc ->
       match acc with
       | Some _ -> acc
       | None ->
         let layout_names = List.map (fun v -> v.vname) layout.e_variants in
         if layout_names = variant_names then Some (name, layout) else None)
    !enum_layouts
    None
;;

(* --- Environment --- *)

type env_entry =
  { offset : int
  ; vtype : value_type
  }

type env = env_entry StringMap.t

let env_lookup name env = StringMap.find_opt name env
let env_add name entry env = StringMap.add name entry env

(* --- String interning --- *)

let intern_string emitter s =
  match Hashtbl.find_opt string_table s with
  | Some lbl -> lbl
  | None ->
    let lbl = Printf.sprintf "str_%d" !string_counter in
    incr string_counter;
    Hashtbl.add string_table s lbl;
    let bytes =
      s |> String.to_seq |> List.of_seq |> List.map (fun c -> string_of_int (Char.code c))
    in
    let data = String.concat ", " (bytes @ [ "0" ]) in
    Emitter.emit_data emitter (Printf.sprintf "%s: db %s" lbl data);
    lbl
;;

(* --- Expression emission returning value_type --- *)

let rec emit_expression emitter env expr =
  let rec emit_relational = function
    | Ast.Relational_val add -> emit_additive add
    | Ast.Eql (l, r) ->
      let* _ = emit_additive l in
      emit_instr emitter "push rax";
      let* _ = emit_additive r in
      emit_instr emitter "pop rbx";
      emit_instr emitter "cmp rbx, rax";
      emit_instr emitter "sete al";
      emit_instr emitter "movzx rax, al";
      Result.Ok Type_int
    | Ast.Neq (l, r) ->
      let* _ = emit_additive l in
      emit_instr emitter "push rax";
      let* _ = emit_additive r in
      emit_instr emitter "pop rbx";
      emit_instr emitter "cmp rbx, rax";
      emit_instr emitter "setne al";
      emit_instr emitter "movzx rax, al";
      Result.Ok Type_int
    | Ast.Lt (l, r) ->
      let* _ = emit_additive l in
      emit_instr emitter "push rax";
      let* _ = emit_additive r in
      emit_instr emitter "pop rbx";
      emit_instr emitter "cmp rbx, rax";
      emit_instr emitter "setl al";
      emit_instr emitter "movzx rax, al";
      Result.Ok Type_int
    | Ast.Gt (l, r) ->
      let* _ = emit_additive l in
      emit_instr emitter "push rax";
      let* _ = emit_additive r in
      emit_instr emitter "pop rbx";
      emit_instr emitter "cmp rbx, rax";
      emit_instr emitter "setg al";
      emit_instr emitter "movzx rax, al";
      Result.Ok Type_int
    | Ast.Leq (l, r) ->
      let* _ = emit_additive l in
      emit_instr emitter "push rax";
      let* _ = emit_additive r in
      emit_instr emitter "pop rbx";
      emit_instr emitter "cmp rbx, rax";
      emit_instr emitter "setle al";
      emit_instr emitter "movzx rax, al";
      Result.Ok Type_int
    | Ast.Geq (l, r) ->
      let* _ = emit_additive l in
      emit_instr emitter "push rax";
      let* _ = emit_additive r in
      emit_instr emitter "pop rbx";
      emit_instr emitter "cmp rbx, rax";
      emit_instr emitter "setge al";
      emit_instr emitter "movzx rax, al";
      Result.Ok Type_int
  and emit_match_expr emitter env (target, arms) =
    let rec extract_atom = function
      | Ast.Relational_expr (Ast.Relational_val a) -> extract_add a
      | _ -> None
    and extract_add = function
      | Ast.Additive_val m -> extract_mul m
      | _ -> None
    and extract_mul = function
      | Ast.Multiplicative_val u -> extract_unary u
      | _ -> None
    and extract_unary = function
      | Ast.Unary_val a -> Some a
      | _ -> None
    in
    let* _ = emit_expression emitter env target in
    emit_instr emitter "mov r13, rax";
    let end_label = gensym "match_end" in
    let rec emit_arms = function
      | [] ->
        (* no match *)
        emit_instr emitter "mov rax, 60";
        emit_instr emitter "mov rdi, 1";
        emit_instr emitter "syscall";
        Result.Ok ()
      | (param, guard_opt, (stmts, expr_opt)) :: rest ->
        let next_label = gensym "match_next" in
        let* () =
          match param with
          | Ast.Single expr ->
            (match extract_atom expr with
             | Some (Ast.Int n) ->
               emit_instr emitter (Printf.sprintf "cmp r13, %d" n);
               emit_instr emitter (Printf.sprintf "jne %s" next_label);
               Result.Ok ()
             | Some (Ast.Ident "_") -> Result.Ok ()
             | _ ->
               let* () = unsupported emitter "pattern not supported" in
               Result.Ok ())
          | _ ->
            let* () = unsupported emitter "pattern not supported" in
            Result.Ok ()
        in
        let* () =
          match guard_opt with
          | Some g ->
            let* _ = emit_expression emitter env g in
            emit_instr emitter "test rax, rax";
            emit_instr emitter (Printf.sprintf "jz %s" next_label);
            Result.Ok ()
          | None -> Result.Ok ()
        in
        let rec emit_stmts = function
          | [] -> Result.Ok ()
          | stmt :: rest ->
            let* () =
              match stmt with
              | Ast.Expression_stmt e ->
                let* _ = emit_expression emitter env e in
                Result.Ok ()
              | _ -> unsupported emitter "stmt in match arm not supported"
            in
            emit_stmts rest
        in
        let* () = emit_stmts stmts in
        let* () =
          match expr_opt with
          | Some e ->
            let* _ = emit_expression emitter env e in
            Result.Ok ()
          | None -> Result.Ok ()
        in
        emit_instr emitter (Printf.sprintf "jmp %s" end_label);
        emit_label emitter next_label;
        emit_arms rest
    in
    let* () = emit_arms arms in
    emit_label emitter end_label;
    Result.Ok Type_unknown
  and emit_additive = function
    | Ast.Add (l, r) ->
      let* _ = emit_additive l in
      emit_instr emitter "push rax";
      let* _ = emit_multiplicative r in
      emit_instr emitter "pop rbx";
      emit_instr emitter "add rax, rbx";
      Result.Ok Type_int
    | Ast.Sub (l, r) ->
      let* _ = emit_additive l in
      emit_instr emitter "push rax";
      let* _ = emit_multiplicative r in
      emit_instr emitter "pop rbx";
      emit_instr emitter "mov rcx, rax";
      emit_instr emitter "mov rax, rbx";
      emit_instr emitter "sub rax, rcx";
      Result.Ok Type_int
    | Ast.Additive_val m -> emit_multiplicative m
  and emit_multiplicative = function
    | Ast.Mul (l, r) ->
      let* _ = emit_multiplicative l in
      emit_instr emitter "push rax";
      let* _ = emit_unary r in
      emit_instr emitter "pop rbx";
      emit_instr emitter "imul rax, rbx";
      Result.Ok Type_int
    | Ast.Div (l, r) ->
      let* _ = emit_multiplicative l in
      emit_instr emitter "push rax";
      let* _ = emit_unary r in
      emit_instr emitter "pop rbx";
      emit_instr emitter "mov rcx, rax";
      emit_instr emitter "mov rax, rbx";
      emit_instr emitter "cqo";
      emit_instr emitter "idiv rcx";
      Result.Ok Type_int
    | Ast.Mod (l, r) ->
      let* _ = emit_multiplicative l in
      emit_instr emitter "push rax";
      let* _ = emit_unary r in
      emit_instr emitter "pop rbx";
      emit_instr emitter "mov rcx, rax";
      emit_instr emitter "mov rax, rbx";
      emit_instr emitter "cqo";
      emit_instr emitter "idiv rcx";
      emit_instr emitter "mov rax, rdx";
      Result.Ok Type_int
    | Ast.Pow (l, r) ->
      (* Implement power using repeated multiplication loop
         Algorithm: result = 1; while (exp > 0) { result *= base; exp--; }
         For negative exponents, result is 0 (integer division) *)
      let* _ = emit_multiplicative l in
      emit_instr emitter "push rax";  (* save base *)
      let* _ = emit_unary r in
      emit_instr emitter "mov rcx, rax";  (* rcx = exponent *)
      emit_instr emitter "pop rbx";  (* rbx = base *)
      emit_instr emitter "mov rax, 1";  (* rax = result = 1 *)
      emit_instr emitter "test rcx, rcx";  (* check if exp <= 0 *)
      let end_label = gensym "pow_end" in
      let loop_label = gensym "pow_loop" in
      emit_instr emitter (Printf.sprintf "jle %s" end_label);  (* if exp <= 0, done *)
      emit_label emitter loop_label;
      emit_instr emitter "imul rax, rbx";  (* result *= base *)
      emit_instr emitter "dec rcx";  (* exp-- *)
      emit_instr emitter (Printf.sprintf "jnz %s" loop_label);  (* loop if exp != 0 *)
      emit_label emitter end_label;
      Result.Ok Type_int
    | Ast.Multiplicative_val u -> emit_unary u
  and emit_unary = function
    | Ast.Neg u ->
      let* _ = emit_unary u in
      emit_instr emitter "neg rax";
      Result.Ok Type_int
    | Ast.Not u ->
      let* _ = emit_unary u in
      emit_instr emitter "cmp rax, 0";
      emit_instr emitter "sete al";
      emit_instr emitter "movzx rax, al";
      Result.Ok Type_bool
    | Ast.Unary_member (base, field) -> emit_member_access emitter env base field
    | Ast.Unary_call call -> emit_call emitter env call
    | Ast.Unary_val a -> emit_atom a
  and emit_atom = function
    | Ast.Bool true ->
      emit_instr emitter "mov rax, 1";
      Result.Ok Type_bool
    | Ast.Bool false ->
      emit_instr emitter "mov rax, 0";
      Result.Ok Type_bool
    | Ast.Int i ->
      emit_instr emitter (Printf.sprintf "mov rax, %d" i);
      Result.Ok Type_int
    | Ast.String s ->
      let lbl = intern_string emitter s in
      emit_instr emitter (Printf.sprintf "lea rax, [rel %s]" lbl);
      Result.Ok Type_string
    | Ast.Unit_val ->
      emit_instr emitter "mov rax, 0";
      Result.Ok Type_unit
    | Ast.Char c ->
      emit_instr emitter (Printf.sprintf "mov rax, %d" (Char.code c));
      Result.Ok Type_char
    | Ast.Grouping e -> emit_expression emitter env e
    | Ast.Ident name ->
      (match env_lookup name env with
       | Some { offset; vtype } ->
         emit_instr emitter (Printf.sprintf "mov rax, [rbp-%d]" offset);
         Result.Ok vtype
       | None ->
         let receiver =
           env
           |> StringMap.bindings
           |> List.find_opt (fun (_, { offset; vtype }) ->
             offset = field_offset 0
             &&
             match vtype with
             | Type_struct _ | Type_enum _ -> true
             | _ -> false)
         in
         (match receiver with
          | Some (_, { offset; vtype = Type_struct sname }) ->
            (match StringMap.find_opt sname !struct_layouts with
             | Some layout ->
               (match List.find_opt (fun f -> f.fname = name) layout.s_fields with
                | Some fl ->
                  emit_instr emitter (Printf.sprintf "mov rax, [rbp-%d]" offset);
                  emit_instr
                    emitter
                    (Printf.sprintf "mov rax, [rax+%d]" (fl.foffset - Target.ptr_size));
                  Result.Ok fl.ftype
                | None ->
                  let* () = unsupported emitter ("unknown field on receiver: " ^ name) in
                  Result.Ok Type_unknown)
             | None ->
               let* () = unsupported emitter "receiver layout not found" in
               Result.Ok Type_unknown)
          | Some (_, { offset; vtype = Type_enum ename }) ->
            (match StringMap.find_opt ename !enum_layouts with
             | Some layout ->
               (match List.find_opt (fun f -> f.fname = name) layout.e_shared with
                | Some fl ->
                  emit_instr emitter (Printf.sprintf "mov rax, [rbp-%d]" offset);
                  emit_instr emitter (Printf.sprintf "mov rax, [rax+%d]" fl.foffset);
                  Result.Ok fl.ftype
                | None ->
                  let* () = unsupported emitter "enum payload lookup not supported" in
                  Result.Ok Type_unknown)
             | None ->
               let* () = unsupported emitter "receiver layout not found" in
               Result.Ok Type_unknown)
          | _ ->
            let* () = unsupported emitter ("identifier lookup not supported: " ^ name) in
            Result.Ok Type_unknown))
    | Ast.Implicit_member name ->
      let* () = unsupported emitter ("implicit member not supported: " ^ name) in
      Result.Ok Type_unknown
  and emit_member_access emitter _env base field =
    let* btype = emit_unary base in
    match btype with
    | Type_struct sname when StringMap.mem sname !enum_layouts ->
      (* Treat struct-typed values whose name matches an enum as enum instances. *)
      emit_enum_member_access emitter sname field
    | Type_struct sname -> emit_struct_member_access emitter sname field
    | Type_enum ename -> emit_enum_member_access emitter ename field
    | _ ->
      let* () = unsupported emitter "member access on non-aggregate" in
      Result.Ok Type_unknown
  and emit_struct_member_access emitter sname field =
    match StringMap.find_opt sname !struct_layouts with
    | None ->
      let* () = unsupported emitter ("unknown struct layout: " ^ sname) in
      Result.Ok Type_unknown
    | Some layout ->
      (match List.find_opt (fun f -> f.fname = field) layout.s_fields with
       | None ->
         let* () = unsupported emitter ("unknown field: " ^ field) in
         Result.Ok Type_unknown
       | Some fl ->
         emit_instr
           emitter
           (Printf.sprintf "mov rax, [rax+%d]" (fl.foffset - Target.ptr_size));
         Result.Ok fl.ftype)
  and emit_enum_member_access emitter ename field =
    match StringMap.find_opt ename !enum_layouts with
    | None ->
      let* () = unsupported emitter ("unknown enum layout: " ^ ename) in
      Result.Ok Type_unknown
    | Some layout ->
      (match List.find_opt (fun f -> f.fname = field) layout.e_shared with
       | Some fl ->
         emit_instr emitter (Printf.sprintf "mov rax, [rax+%d]" fl.foffset);
         Result.Ok fl.ftype
       | None ->
         let* () =
           unsupported emitter ("payload member access not supported yet: " ^ field)
         in
         Result.Ok Type_unknown)
  and emit_call emitter env = function
    (* Avoid double-calling when the callee itself is already a call with no additional
       arguments, e.g. `(printf(...))()` parsed as nested Decl_call with empty params. *)
    | Ast.Decl_call
        ( Ast.Relational_expr
            (Ast.Relational_val
               (Ast.Additive_val (Ast.Multiplicative_val (Ast.Unary_call inner_call))))
        , [] ) -> emit_call emitter env inner_call
    | Ast.Decl_call (callee, params) ->
      let rec extract_member = function
        | Ast.Relational_expr (Ast.Relational_val a) -> extract_member_add a
        | _ -> None
      and extract_member_add = function
        | Ast.Additive_val m -> extract_member_mul m
        | _ -> None
      and extract_member_mul = function
        | Ast.Multiplicative_val u -> extract_member_unary u
        | _ -> None
      and extract_member_unary = function
        | Ast.Unary_member (base, field) -> Some (base, field)
        | _ -> None
      in
      (match extract_member callee with
       | Some (base, field) ->
         (match base with
          | Ast.Unary_val (Ast.Ident n)
            when StringMap.mem n !struct_layouts || StringMap.mem n !enum_layouts ->
            (* Type-qualified member: let callee logic handle (e.g., enum ctor) *)
            let* ctyp = emit_callee emitter env callee in
            let* () = emit_params emitter env params 0 in
            (match ctyp with
             | `Direct name ->
               if not (StringSet.mem name !defined_functions)
               then Emitter.add_extern emitter name;
               emit_instr emitter ("call " ^ name);
               Result.Ok Type_unknown
             | `Struct_ctor sname -> emit_struct_ctor emitter sname params env
             | `Enum_ctor (ename, vname) -> emit_enum_ctor emitter ename vname params env
             | `Indirect ->
               emit_instr emitter "call r11";
               Result.Ok Type_unknown)
          | _ ->
            let* btype = emit_unary base in
            emit_instr emitter "mov rdi, rax";
            let* () = emit_params emitter env params 1 in
            let symbol =
              match btype with
              | Type_struct s | Type_enum s -> String.concat "_" [ s; field ]
              | _ -> field
            in
            if not (StringSet.mem symbol !defined_functions)
            then Emitter.add_extern emitter symbol;
            emit_instr emitter ("call " ^ symbol);
            Result.Ok Type_unknown)
       | None ->
         let* ctyp = emit_callee emitter env callee in
         let* () = emit_params emitter env params 0 in
         (match ctyp with
          | `Direct name ->
            if not (StringSet.mem name !defined_functions)
            then Emitter.add_extern emitter name;
            emit_instr emitter ("call " ^ name);
            Result.Ok Type_unknown
          | `Struct_ctor sname -> emit_struct_ctor emitter sname params env
          | `Enum_ctor (ename, vname) -> emit_enum_ctor emitter ename vname params env
          | `Indirect ->
            emit_instr emitter "call r11";
            Result.Ok Type_unknown))
    | Ast.Macro_call _ ->
      let* () = unsupported emitter "macro calls are not supported yet" in
      Result.Ok Type_unknown
  and emit_params emitter env params idx =
    match params with
    | [] -> Result.Ok ()
    | Ast.Named _ :: _ -> unsupported emitter "named call parameters are not supported"
    | Ast.Positional e :: rest ->
      if idx >= Array.length arg_registers
      then unsupported emitter "more than 6 call arguments are not supported"
      else
        let* _ = emit_expression emitter env e in
        emit_instr emitter (Printf.sprintf "mov %s, rax" arg_registers.(idx));
        emit_params emitter env rest (idx + 1)
  and emit_callee emitter env callee =
    match callee with
    | Ast.Relational_expr
        (Ast.Relational_val
           (Ast.Additive_val (Ast.Multiplicative_val (Ast.Unary_val (Ast.Ident name)))))
      ->
      (match StringMap.find_opt name !struct_layouts with
       | Some _ -> Result.Ok (`Struct_ctor name)
       | None ->
         (match StringMap.find_opt name !enum_layouts with
          | Some _ -> Result.Ok (`Direct name)
          | None -> Result.Ok (`Direct name)))
    | Ast.Relational_expr
        (Ast.Relational_val
           (Ast.Additive_val
              (Ast.Multiplicative_val
                 (Ast.Unary_member (Ast.Unary_val (Ast.Ident base), field))))) ->
      (match StringMap.find_opt base !enum_layouts with
       | Some _ -> Result.Ok (`Enum_ctor (base, field))
       | None ->
         let* _ = emit_expression emitter env callee in
         emit_instr emitter "mov r11, rax";
         Result.Ok `Indirect)
    | _ ->
      let* _ = emit_expression emitter env callee in
      emit_instr emitter "mov r11, rax";
      Result.Ok `Indirect
  and emit_struct_ctor emitter sname params env =
    match StringMap.find_opt sname !struct_layouts with
    | None ->
      let* () = unsupported emitter ("unknown struct ctor: " ^ sname) in
      Result.Ok Type_unknown
    | Some layout ->
      let argc = List.length layout.s_fields in
      if argc <> List.length params
      then
        let* () = unsupported emitter "arity mismatch for struct ctor" in
        Result.Ok (Type_struct sname)
      else (
        let size = layout.s_size in
        Emitter.add_extern emitter "malloc";
        emit_instr emitter (Printf.sprintf "mov rdi, %d" size);
        emit_instr emitter "call malloc";
        emit_instr emitter "mov r12, rax";
        let rec store_fields idx p fs =
          match p, fs with
          | [], [] -> Result.Ok ()
          | Ast.Positional e :: prest, f :: frest ->
            let* _ = emit_expression emitter env e in
            emit_instr
              emitter
              (Printf.sprintf "mov [r12+%d], rax" (f.foffset - Target.ptr_size));
            store_fields (idx + 1) prest frest
          | _ -> unsupported emitter "parameter/field mismatch"
        in
        let* () = store_fields 0 params layout.s_fields in
        emit_instr emitter "mov rax, r12";
        Result.Ok (Type_struct sname))
  and emit_enum_ctor emitter ename vname params env =
    match StringMap.find_opt ename !enum_layouts with
    | None ->
      let* () = unsupported emitter ("unknown enum ctor: " ^ ename) in
      Result.Ok Type_unknown
    | Some layout ->
      let sharedc = List.length layout.e_shared in
      let variant = List.find_opt (fun v -> v.vname = vname) layout.e_variants in
      (match variant with
       | None ->
         let* () = unsupported emitter ("unknown variant: " ^ vname) in
         Result.Ok Type_unknown
       | Some vlayout ->
         let expected = sharedc + List.length vlayout.v_payload in
         if expected <> List.length params
         then
           let* () = unsupported emitter "arity mismatch for enum ctor" in
           Result.Ok (Type_enum ename)
         else (
           let size =
             Target.ptr_size + (sharedc * Target.ptr_size) + layout.e_payload_size
           in
           Emitter.add_extern emitter "malloc";
           emit_instr emitter (Printf.sprintf "mov rdi, %d" size);
           emit_instr emitter "call malloc";
           emit_instr emitter "mov r12, rax";
           (* tag *)
           let tag =
             let rec idx i = function
               | [] -> 0
               | v :: rest -> if v.vname = vname then i else idx (i + 1) rest
             in
             idx 0 layout.e_variants
           in
           emit_instr emitter (Printf.sprintf "mov qword [r12], %d" tag);
           let rec take n xs =
             if n = 0
             then []
             else (
               match xs with
               | [] -> []
               | x :: rest -> x :: take (n - 1) rest)
           in
           let rec drop n xs =
             if n = 0
             then xs
             else (
               match xs with
               | [] -> []
               | _ :: rest -> drop (n - 1) rest)
           in
           let rec store_shared ps fs =
             match ps, fs with
             | [], [] -> Result.Ok ()
             | Ast.Positional e :: prest, f :: frest ->
               let* _ = emit_expression emitter env e in
               emit_instr emitter (Printf.sprintf "mov [r12+%d], rax" f.foffset);
               store_shared prest frest
             | _ -> unsupported emitter "shared field mismatch"
           in
           let rec store_payload ps payload =
             match ps, payload with
             | [], [] -> Result.Ok ()
             | Ast.Positional e :: prest, f :: frest ->
               let* _ = emit_expression emitter env e in
               emit_instr emitter (Printf.sprintf "mov [r12+%d], rax" f.foffset);
               store_payload prest frest
             | _ -> unsupported emitter "payload field mismatch"
           in
           let shared_args = take sharedc params in
           let payload_args = drop sharedc params in
           let* () = store_shared shared_args layout.e_shared in
           let* () = store_payload payload_args vlayout.v_payload in
           emit_instr emitter "mov rax, r12";
           Result.Ok (Type_enum ename)))
  in
  let emit_struct_literal (fields, _with_block) =
    let name, layout =
      match find_struct_layout_by_fields fields with
      | Some found -> found
      | None ->
        let n = Printf.sprintf "__anon_struct_%d" !anon_struct_counter in
        incr anon_struct_counter;
        let layout = build_struct_layout n fields in
        struct_layouts := StringMap.add n layout !struct_layouts;
        n, layout
    in
    let size = layout.s_size in
    Emitter.add_extern emitter "malloc";
    emit_instr emitter (Printf.sprintf "mov rdi, %d" size);
    emit_instr emitter "call malloc";
    emit_instr emitter "mov r12, rax";
    let find_field_expr fname =
      match List.find_opt (fun (n, _, _) -> n = fname) fields with
      | Some (_, _, Some e) -> Some e
      | _ -> None
    in
    let rec store = function
      | [] -> Result.Ok ()
      | f :: rest ->
        let* () =
          match find_field_expr f.fname with
          | Some e ->
            let* _ = emit_expression emitter env e in
            Result.Ok ()
          | None ->
            emit_instr emitter "mov rax, 0";
            Result.Ok ()
        in
        emit_instr
          emitter
          (Printf.sprintf "mov [r12+%d], rax" (f.foffset - Target.ptr_size));
        store rest
    in
    let* () = store layout.s_fields in
    emit_instr emitter "mov rax, r12";
    Result.Ok (Type_struct name)
  in
  let emit_enum_literal (variants, _with_block) =
    let name, layout =
      match find_enum_layout_by_variants variants with
      | Some found -> found
      | None ->
        let n = Printf.sprintf "__anon_enum_%d" !anon_enum_counter in
        incr anon_enum_counter;
        let layout = build_enum_layout n [] variants in
        enum_layouts := StringMap.add n layout !enum_layouts;
        n, layout
    in
    let size =
      Target.ptr_size
      + (List.length layout.e_shared * Target.ptr_size)
      + layout.e_payload_size
    in
    Emitter.add_extern emitter "malloc";
    emit_instr emitter (Printf.sprintf "mov rdi, %d" size);
    emit_instr emitter "call malloc";
    emit_instr emitter "mov r12, rax";
    emit_instr emitter "mov qword [r12], 0";
    emit_instr emitter "mov rax, 0";
    let rec clear_shared = function
      | [] -> Result.Ok ()
      | f :: rest ->
        emit_instr emitter (Printf.sprintf "mov [r12+%d], rax" f.foffset);
        clear_shared rest
    in
    let rec clear_payload off bytes =
      if bytes <= 0
      then Result.Ok ()
      else (
        emit_instr emitter (Printf.sprintf "mov [r12+%d], rax" off);
        clear_payload (off + Target.ptr_size) (bytes - Target.ptr_size))
    in
    let* () = clear_shared layout.e_shared in
    let* () =
      clear_payload
        (Target.ptr_size + (List.length layout.e_shared * Target.ptr_size))
        layout.e_payload_size
    in
    emit_instr emitter "mov rax, r12";
    Result.Ok (Type_enum name)
  in
  match expr with
  | Ast.Call_expr call -> emit_call emitter env call
  | Ast.Relational_expr rel -> emit_relational rel
  | Ast.Match_expr m -> emit_match_expr emitter env m
  | Ast.Struct_expr (fields, with_block) -> emit_struct_literal (fields, with_block)
  | Ast.Enum_expr (variants, with_block) -> emit_enum_literal (variants, with_block)
  | Ast.Macro_expr _ ->
    let* () = unsupported emitter "macro expressions are not supported yet" in
    Result.Ok Type_unknown
  | Ast.Derive_expr _ ->
    let* () = unsupported emitter "derive expressions are not supported yet" in
    Result.Ok Type_unknown
;;

(* --- Return and statements --- *)

let emit_return emitter env end_label = function
  | Ast.With_expr expr ->
    let* _ = emit_expression emitter env expr in
    emit_instr emitter ("jmp " ^ end_label);
    Result.Ok ()
  | Ast.Naked ->
    emit_instr emitter "xor rax, rax";
    emit_instr emitter ("jmp " ^ end_label);
    Result.Ok ()
;;

let mangle prefix name = String.concat "_" (prefix @ [ name ])

let emit_function_stub emitter symbol =
  Emitter.add_global emitter symbol;
  emit_label emitter symbol;
  emit_instr emitter "push rbp";
  emit_instr emitter "mov rbp, rsp";
  emit_instr emitter "; stub: abort (unimplemented)";
  emit_instr emitter (Printf.sprintf "mov rax, %d" Target.exit_syscall);
  emit_instr emitter "mov rdi, 1";
  emit_instr emitter "syscall";
  emit_instr emitter "ud2";
  emit_instr emitter "mov rsp, rbp";
  emit_instr emitter "pop rbp";
  emit_instr emitter "ret";
  Emitter.emit_text emitter ""
;;

let rec emit_decl emitter prefix = function
  | Ast.Decl { name; params; body; _ } ->
    let symbol = mangle prefix name in
    (match snd body with
     | Some (Ast.Struct_expr (_, Some w)) | Some (Ast.Enum_expr (_, Some w)) ->
       emit_nodes emitter (prefix @ [ name ]) w
     | _ -> Result.Ok ())
    |> fun _ -> emit_function emitter prefix name params symbol body
  | Ast.Module_decl { name; body; _ } -> emit_nodes emitter (prefix @ [ name ]) body
  | Ast.Curry_decl { name; _ } ->
    let symbol = mangle prefix name in
    emit_function_stub emitter symbol;
    Result.Ok ()
  | Ast.Import_decl { name; _ } ->
    let symbol = mangle prefix name in
    emit_function_stub emitter symbol;
    Result.Ok ()
  | Ast.Export_stmt _ -> Result.Ok ()

and emit_nodes emitter prefix = function
  | [] -> Result.Ok ()
  | Ast.Statement stmt :: rest ->
    let* () = emit_statement emitter prefix [] stmt in
    emit_nodes emitter prefix rest
  | _ :: rest -> emit_nodes emitter prefix rest

and emit_statement emitter prefix _env_stack = function
  | Ast.Decl_stmt decl ->
    (* Top-level decls handled in emit_nodes; locals handled in emit_function *)
    emit_decl emitter prefix decl
  | Ast.Return_stmt _ -> unsupported emitter "return outside function"
  | Ast.Open_stmt _ -> Result.Ok ()
  | Ast.If_stmt _ -> unsupported emitter "if statements at top-level not supported"
  | Ast.While_stmt _ -> unsupported emitter "while statements at top-level not supported"
  | Ast.Expression_stmt _ -> Result.Ok ()

and emit_statement_in_fn emitter prefix env end_label locals_ref = function
  | Ast.Return_stmt r ->
    let* () = emit_return emitter env end_label r in
    Result.Ok env
  | Ast.Expression_stmt expr ->
    let* _ = emit_expression emitter env expr in
    Result.Ok env
  | Ast.Open_stmt _ -> Result.Ok env
  | Ast.If_stmt { cond; body; elif } ->
    let rec emit_block env = function
      | [] -> Result.Ok env
      | Ast.Statement s :: rest ->
        let* env' = emit_statement_in_fn emitter prefix env end_label locals_ref s in
        emit_block env' rest
      | Ast.Expression e :: rest ->
        let* _ = emit_expression emitter env e in
        emit_block env rest
      | Ast.Error _ :: rest -> emit_block env rest
    in
    let rec emit_else env = function
      | Ast.Else_if (e, blk, rest) ->
        let next_label = gensym "elif" in
        let end_label = gensym "endif" in
        let* _ = emit_expression emitter env e in
        emit_instr emitter "cmp rax, 0";
        emit_instr emitter ("je " ^ next_label);
        let* _env_then = emit_block env blk in
        emit_instr emitter ("jmp " ^ end_label);
        emit_label emitter next_label;
        let* env_rest = emit_else env rest in
        emit_label emitter end_label;
        Result.Ok env_rest
      | Ast.Else blk -> emit_block env blk
      | Ast.Nope -> Result.Ok env
    in
    let else_label = gensym "else" in
    let end_label_local = gensym "endif" in
    let* _ = emit_expression emitter env cond in
    emit_instr emitter "cmp rax, 0";
    emit_instr emitter ("je " ^ else_label);
    let* _env_then = emit_block env body in
    emit_instr emitter ("jmp " ^ end_label_local);
    emit_label emitter else_label;
    let* env_else = emit_else env elif in
    emit_label emitter end_label_local;
    Result.Ok env_else
  | Ast.While_stmt { cond; body } ->
    let start_label = gensym "while_start" in
    let end_label = gensym "while_end" in
    emit_label emitter start_label;
    let* _ = emit_expression emitter env cond in
    emit_instr emitter "cmp rax, 0";
    emit_instr emitter ("je " ^ end_label);
    let rec emit_block env = function
      | [] -> Result.Ok env
      | Ast.Statement s :: rest ->
        let* env' = emit_statement_in_fn emitter prefix env end_label locals_ref s in
        emit_block env' rest
      | Ast.Expression e :: rest ->
        let* _ = emit_expression emitter env e in
        emit_block env rest
      | Ast.Error _ :: rest -> emit_block env rest
    in
    let* _env_body = emit_block env body in
    emit_instr emitter ("jmp " ^ start_label);
    emit_label emitter end_label;
    Result.Ok env
  | Ast.Decl_stmt (Ast.Decl { name; params = []; body = stmts, expr_opt; _ })
    when stmts = [] ->
    (* local let binding *)
    let* vtype =
      match expr_opt with
      | Some e -> emit_expression emitter env e
      | None -> Result.Ok Type_unit
    in
    let offset = (!locals_ref + 1) * Target.ptr_size in
    emit_instr emitter (Printf.sprintf "mov [rbp-%d], rax" offset);
    locals_ref := !locals_ref + 1;
    let env = env_add name { offset; vtype } env in
    Result.Ok env
  | Ast.Decl_stmt decl ->
    let nested_prefix = prefix in
    let* () = emit_decl emitter nested_prefix decl in
    Result.Ok env

and emit_function emitter prefix name params symbol (stmts, expr_opt) =
  let nested_prefix = prefix @ [ name ] in
  let rec count_locals_in_t acc = function
    | [] -> acc
    | Ast.Statement s :: rest -> count_locals_in_t (count_locals_in_stmt acc s) rest
    | _ :: rest -> count_locals_in_t acc rest
  and count_locals_in_stmt acc = function
    | Ast.Decl_stmt (Ast.Decl { params = []; body = stmts, _; _ }) when stmts = [] ->
      acc + 1
    | Ast.If_stmt { body; elif; _ } ->
      count_locals_in_else (count_locals_in_t acc body) elif
    | Ast.While_stmt { body; _ } -> count_locals_in_t acc body
    | _ -> acc
  and count_locals_in_else acc = function
    | Ast.Nope -> acc
    | Ast.Else tlist -> count_locals_in_t acc tlist
    | Ast.Else_if (_e, tlist, rest) ->
      let acc' = count_locals_in_t acc tlist in
      count_locals_in_else acc' rest
  in
  let rec build_params idx env = function
    | [] -> Result.Ok (idx, env)
    | Ast.Untyped id :: rest ->
      if idx >= Array.length arg_registers
      then
        let* () = unsupported emitter "more than 6 parameters are not supported" in
        Result.Ok (idx, env)
      else (
        let recv_type =
          match prefix with
          | [ type_name ] | _ :: type_name :: _ ->
            if StringMap.mem type_name !struct_layouts
            then Type_struct type_name
            else if StringMap.mem type_name !enum_layouts
            then Type_enum type_name
            else Type_unknown
          | _ -> Type_unknown
        in
        let entry =
          if idx = 0
          then { offset = field_offset idx; vtype = recv_type }
          else { offset = field_offset idx; vtype = Type_unknown }
        in
        build_params (idx + 1) (env_add id entry env) rest)
    | Ast.Typed (id, t) :: rest ->
      if idx >= Array.length arg_registers
      then
        let* () = unsupported emitter "more than 6 parameters are not supported" in
        Result.Ok (idx, env)
      else (
        let entry = { offset = field_offset idx; vtype = type_of_typ t } in
        build_params (idx + 1) (env_add id entry env) rest)
    | Ast.OptionalTyped _ :: _ | Ast.OptionalUntyped _ :: _ | Ast.Variadic _ :: _ ->
      let* () = unsupported emitter "optional/variadic parameters are not supported" in
      Result.Ok (idx, env)
  in
  let receiver_type prefix =
    match prefix with
    | [] -> None
    | [ type_name ] | _ :: type_name :: _ ->
      if StringMap.mem type_name !struct_layouts
      then Some (Type_struct type_name)
      else if StringMap.mem type_name !enum_layouts
      then Some (Type_enum type_name)
      else None
  in
  let* param_count, env = build_params 0 StringMap.empty params in
  let param_count, env =
    match param_count, receiver_type prefix with
    | 0, Some rtype ->
      let offset = field_offset 0 in
      1, env_add "__self" { offset; vtype = rtype } env
    | _ -> param_count, env
  in
  let local_slots = ref 0 in
  let estimated_locals =
    count_locals_in_t 0 (List.map (fun s -> Ast.Statement s) stmts)
  in
  let stack_bytes = (param_count + estimated_locals) * Target.ptr_size in
  let stack_alloc = if stack_bytes = 0 then 0 else (stack_bytes + 15) / 16 * 16 in
  (* Printf.eprintf
    "[codegen] %s locals=%d params=%d stack_alloc=%d\n"
    symbol
    estimated_locals
    param_count
    stack_alloc; *)
  Emitter.add_global emitter symbol;
  emit_label emitter symbol;
  emit_instr emitter "push rbp";
  emit_instr emitter "mov rbp, rsp";
  emit_instr emitter (Printf.sprintf "; stack alloc %d" stack_alloc);
  if stack_alloc > 0 then emit_instr emitter (Printf.sprintf "sub rsp, %d" stack_alloc);
  for i = 0 to param_count - 1 do
    emit_instr
      emitter
      (Printf.sprintf "mov [rbp-%d], %s" (field_offset i) arg_registers.(i))
  done;
  let end_label = symbol ^ "_end" in
  let rec emit_body env = function
    | [] -> Result.Ok env
    | stmt :: rest ->
      let* env' =
        emit_statement_in_fn emitter nested_prefix env end_label local_slots stmt
      in
      emit_body env' rest
  in
  let* env = emit_body env stmts in
  let* () =
    match expr_opt with
    | Some expr ->
      let* _ = emit_expression emitter env expr in
      Result.Ok ()
    | None -> Result.Ok ()
  in
  emit_label emitter end_label;
  emit_instr emitter "mov rsp, rbp";
  emit_instr emitter "pop rbp";
  emit_instr emitter "ret";
  Emitter.emit_text emitter "";
  Result.Ok ()
;;

let rec collect_functions_from_with prefix acc = function
  | [] -> acc
  | Ast.Statement stmt :: rest ->
    let acc' = collect_from_statement prefix acc stmt in
    collect_functions_from_with prefix acc' rest
  | Ast.Expression _ :: rest -> collect_functions_from_with prefix acc rest
  | Ast.Error _ :: rest -> collect_functions_from_with prefix acc rest

and collect_functions prefix acc = function
  | [] -> acc
  | Ast.Statement stmt :: rest ->
    let acc' = collect_from_statement prefix acc stmt in
    collect_functions prefix acc' rest
  | Ast.Expression _ :: rest -> collect_functions prefix acc rest
  | Ast.Error _ :: rest -> collect_functions prefix acc rest

and collect_from_body prefix acc (stmts, expr_opt) =
  let acc =
    List.fold_left (fun acc stmt -> collect_from_statement prefix acc stmt) acc stmts
  in
  match expr_opt with
  | Some (Ast.Struct_expr (_, Some w)) | Some (Ast.Enum_expr (_, Some w)) ->
    collect_functions_from_with prefix acc w
  | _ -> acc

and collect_from_statement prefix acc = function
  | Ast.Decl_stmt (Ast.Decl { name; body; _ }) ->
    let symbol = mangle prefix name in
    let acc' = StringSet.add symbol acc in
    let nested_prefix = prefix @ [ name ] in
    collect_from_body nested_prefix acc' body
  | Ast.Decl_stmt (Ast.Module_decl { name; body; _ }) ->
    let prefix' = prefix @ [ name ] in
    collect_functions prefix' acc body
  | Ast.Decl_stmt (Ast.Curry_decl { name; _ }) -> StringSet.add (mangle prefix name) acc
  | Ast.Decl_stmt (Ast.Import_decl { name; _ }) -> StringSet.add (mangle prefix name) acc
  | Ast.Decl_stmt (Ast.Export_stmt _) -> acc
  | Ast.Open_stmt _
  | Ast.Return_stmt _
  | Ast.If_stmt _
  | Ast.While_stmt _
  | Ast.Expression_stmt _ -> acc
;;

let generate_code nodes =
  match ensure_semantics nodes with
  | Result.Error err -> Result.Error err
  | Result.Ok () ->
    collect_layouts nodes;
    let emitter = Emitter.create () in
    Hashtbl.reset string_table;
    string_counter := 0;
    let functions = collect_functions [] StringSet.empty nodes in
    defined_functions := functions;
    let* () = emit_nodes emitter [] nodes in
    Result.Ok (Emitter.render emitter)
;;
