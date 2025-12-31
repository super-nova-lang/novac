let%expect_test "codegen_examples" =
  List.iter
    (fun (name, content) ->
       Codegen.reset_module ();
       Codegen.set_module_name name;
       let tokens = Lexer.lex (name ^ ".nova") content in
       let nodes = Parser.parse (Parser.create tokens) in
       let errors, _warnings = Analysis.analyze nodes in
       if errors <> []
       then
         failwith
           ("analysis failed for "
            ^ name
            ^ ": "
            ^ String.concat ", "
                (List.map
                   (function
                     | Analysis.Error (Analysis.Undefined_variable (v, _)) ->
                       "undefined " ^ v
                     | Analysis.Error (Analysis.Type_mismatch _) -> "type mismatch"
                     | Analysis.Error (Analysis.Duplicate_declaration (v, _)) ->
                       "duplicate " ^ v
                     | Analysis.Error (Analysis.Invalid_operation msg) -> msg
                     | Analysis.Error (Analysis.Missing_return_type f) ->
                       "missing return for " ^ f
                     | Analysis.Warning _ -> "warning")
                   errors));
       Codegen.set_analysis_nodes nodes;
       List.iter Codegen.codegen nodes;
       Codegen.finish_module ();
       Format.printf "codegen %s ok\n" name)
    Nova_tests.all;
  [%expect {|
codegen showcase ok
|}]
