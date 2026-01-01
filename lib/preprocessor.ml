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

(* For now, minimal macro support - just expand to empty *)
let preprocess nodes =
  (* First pass: collect all macro definitions *)
  Hashtbl.reset macro_table;
  collect_macros nodes;
  (* For now, just return nodes as-is *)
  (* Macro expansion will be added in the future *)
  nodes
;;
