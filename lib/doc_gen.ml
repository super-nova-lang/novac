type doc_out =
  | Stdout
  | Html

let build_dir = "build"
let doc_dir = Filename.concat build_dir "doc"

let ensure_dir dir =
  if not (Sys.file_exists dir && Sys.is_directory dir) then Unix.mkdir dir 0o755
;;

let string_of_doc ~doc_files ~out_type =
  let html_escape s =
    let b = Buffer.create (String.length s) in
    String.iter
      (function
        | '&' -> Buffer.add_string b "&amp;"
        | '<' -> Buffer.add_string b "&lt;"
        | '>' -> Buffer.add_string b "&gt;"
        | '"' -> Buffer.add_string b "&quot;"
        | '\'' -> Buffer.add_string b "&#39;"
        | c -> Buffer.add_char b c)
      s;
    Buffer.contents b
  in
  let rec show_typ = function
    | Ast.User id -> id
    | Ast.Generic (id, args) ->
      let args_s = args |> List.map show_typ |> String.concat ", " in
      Printf.sprintf "%s[%s]" id args_s
    | Ast.Type_var id -> id
    | Ast.Builtin id -> id
    | Ast.Unit_typ -> "()"
    | Ast.List_typ t -> Printf.sprintf "[%s]" (show_typ t)
  in
  let param_to_string = function
    | Ast.Untyped id -> id
    | Ast.Typed (id, t) -> Printf.sprintf "%s: %s" id (show_typ t)
    | Ast.OptionalTyped (id, t, _) -> Printf.sprintf "?%s: %s" id (show_typ t)
    | Ast.OptionalUntyped (id, _) -> Printf.sprintf "?%s" id
    | Ast.Variadic id -> Printf.sprintf "...%s" id
  in
  let signature_of_decl = function
    | Ast.Decl { name; generics; params; explicit_ret; body = _, expr_opt; _ } ->
      let gens = if generics = [] then "" else "[" ^ String.concat ", " generics ^ "]" in
      let nameg = name ^ gens in
      (match expr_opt with
       | Some (Ast.Struct_expr (fields, _)) ->
         let payload =
           fields |> List.map (fun (_, t, _) -> show_typ t) |> String.concat ", "
         in
         Printf.sprintf "type %s = struct { %s }" nameg payload
       | Some (Ast.Enum_expr (variants, _)) ->
         let variant_s =
           variants
           |> List.map (fun (vname, vbody) ->
             match vbody with
             | None -> vname
             | Some (Ast.Struct_body fields) ->
               let payload =
                 fields |> List.map (fun (_, t, _) -> show_typ t) |> String.concat ", "
               in
               Printf.sprintf "%s (%s)" vname payload
             | Some (Ast.Type_body t) -> Printf.sprintf "%s (%s)" vname (show_typ t))
           |> String.concat ", "
         in
         Printf.sprintf "type %s = enum { %s }" nameg variant_s
       | _ ->
         let params_s = params |> List.map param_to_string |> String.concat ", " in
         let ret_s =
           match explicit_ret with
           | Some ret -> " -> " ^ show_typ ret
           | None -> ""
         in
         Printf.sprintf "%s(%s)%s" nameg params_s ret_s)
    | Ast.Curry_decl { name; curried; _ } -> Printf.sprintf "%s := %s" name curried
    | Ast.Import_decl { name; link_name; _ } ->
      Printf.sprintf "%s (import \"%s\")" name link_name
    | Ast.Module_decl { name; _ } -> Printf.sprintf "module %s" name
    | Ast.Export_stmt (Ast.Export_ident id) -> Printf.sprintf "export %s" id
    | Ast.Export_stmt (Ast.Export_rename (src, dst)) ->
      Printf.sprintf "export %s as %s" src dst
  in
  let collect_signatures nodes =
    let rec collect acc = function
      | [] -> List.rev acc
      | Ast.Statement (Ast.Decl_stmt decl) :: rest ->
        collect (signature_of_decl decl :: acc) rest
      | _ :: rest -> collect acc rest
    in
    collect [] nodes
  in
  let collect_top_level_docs tokens =
    let rec loop depth acc = function
      | [] -> List.rev acc
      | (tok, loc) :: rest ->
        let acc' =
          match tok, depth with
          | Token.Doc_comment s, 0 -> (loc, s) :: acc
          | _ -> acc
        in
        let depth' =
          match tok with
          | Token.Open_brack -> depth + 1
          | Token.Close_brack -> max 0 (depth - 1)
          | _ -> depth
        in
        loop depth' acc' rest
    in
    loop 0 [] tokens
  in
  let buf = Buffer.create 256 in
  let entries =
    doc_files
    |> List.concat_map (fun file ->
      let tokens = Lexer.lex_from_file file in
      let docs = collect_top_level_docs tokens in
      let nodes = Parser.parse (Parser.create tokens) in
      let sigs = collect_signatures nodes in
      let rec pair acc docs sigs =
        match sigs with
        | [] -> List.rev acc
        | sigstr :: sr ->
          let docs', entry =
            match docs with
            | (loc, doc) :: dr -> dr, Some (loc, doc, sigstr)
            | [] -> [], None
          in
          let acc' =
            match entry with
            | Some e -> e :: acc
            | None -> (Token.{ file; row = 0; col = 0 }, "", sigstr) :: acc
          in
          pair acc' docs' sr
      in
      pair [] docs sigs)
  in
  match out_type with
  | Stdout ->
    entries
    |> List.iteri (fun idx (loc, doc, sigstr) ->
      if idx > 0 then Buffer.add_char buf '\n';
      let doc_part = if String.length doc = 0 then "" else doc in
      if doc_part = ""
      then Buffer.add_string buf sigstr
      else
        Buffer.add_string
          buf
          (Printf.sprintf "%s: %s\n\t%s" (Token.show_loc loc) doc_part sigstr));
    Buffer.contents buf
  | Html ->
    ensure_dir build_dir;
    ensure_dir doc_dir;
    let doc_path = Filename.concat doc_dir "index.html" in
    let css_path = Filename.concat doc_dir "style.css" in
    let js_path = Filename.concat doc_dir "script.js" in
    let write_file path content =
      let oc = open_out path in
      Fun.protect (fun () -> output_string oc content) ~finally:(fun () -> close_out oc)
    in
    let style_css =
      String.concat
        "\n"
        [ ":root{--bg:#ffffff;--fg:#111827;--muted:#4b5563;--surface:#f5f5f5;--border:#dddddd;--accent:#2563eb;}"
        ; "body{font-family:sans-serif;max-width:960px;margin:40px auto;padding:0 \
           16px;background:var(--bg);color:var(--fg);transition:background 0.2s \
           ease,color 0.2s ease;}"
        ; "h1{margin:0 0 12px 0;}"
        ; ".controls{display:flex;gap:8px;align-items:center;margin:0 0 20px 0;}"
        ; ".controls label{font-weight:600;}"
        ; ".controls input{flex:1;padding:8px 10px;border:1px solid \
           var(--border);border-radius:6px;background:var(--bg);color:var(--fg);}"
        ; ".controls button{padding:8px 12px;border:1px solid \
           var(--border);background:var(--surface);color:var(--fg);border-radius:6px;cursor:pointer;}"
        ; "section{margin-bottom:24px;border-bottom:1px solid \
           var(--border);padding-bottom:16px;}"
        ; "h2{margin:0 0 4px 0;font-size:1.1rem;}"
        ; ".loc{color:var(--muted);font-size:0.9em;margin:0 0 8px 0;}"
        ; "pre{background:var(--surface);padding:8px;border-radius:4px;white-space:pre-wrap;border:1px \
           solid var(--border);}"
        ; "code{white-space:pre-wrap;}"
        ; "body.dark{--bg:#0f172a;--fg:#e5e7eb;--muted:#94a3b8;--surface:#1f2937;--border:#334155;--accent:#38bdf8;}"
        ]
    in
    let script_js =
      String.concat
        "\n"
        [ "document.addEventListener('DOMContentLoaded', () => {"
        ; "  const input = document.querySelector('#search');"
        ; "  const toggle = document.querySelector('#toggle-theme');"
        ; "  const sections = \
           Array.from(document.querySelectorAll('section[data-signature]'));"
        ; "  if (!input) return;"
        ; "  const normalize = (s) => s.toLowerCase();"
        ; "  const applyTheme = (mode) => {"
        ; "    const body = document.body;"
        ; "    if (mode === 'dark') body.classList.add('dark'); else \
           body.classList.remove('dark');"
        ; "    localStorage.setItem('docs-theme', mode);"
        ; "    if (toggle) toggle.textContent = mode === 'dark' ? 'Light mode' : 'Dark \
           mode';"
        ; "  };"
        ; "  const saved = localStorage.getItem('docs-theme') || 'light';"
        ; "  applyTheme(saved);"
        ; "  const filter = () => {"
        ; "    const term = normalize(input.value.trim());"
        ; "    sections.forEach((section) => {"
        ; "      const sig = normalize(section.dataset.signature || '');"
        ; "      const doc = normalize(section.dataset.doc || '');"
        ; "      const match = term === '' || sig.includes(term) || doc.includes(term);"
        ; "      section.style.display = match ? '' : 'none';"
        ; "    });"
        ; "  };"
        ; "  input.addEventListener('input', filter);"
        ; "  if (toggle) {"
        ; "    toggle.addEventListener('click', () => {"
        ; "      const next = document.body.classList.contains('dark') ? 'light' : \
           'dark';"
        ; "      applyTheme(next);"
        ; "    });"
        ; "  }"
        ; "  filter();"
        ; "});"
        ]
    in
    write_file css_path style_css;
    write_file js_path script_js;
    let oc = open_out doc_path in
    Fun.protect
      (fun () ->
         output_string oc "<!DOCTYPE html><html><head><meta charset=\"utf-8\">";
         output_string oc "<title>Supernova Docs</title>";
         output_string oc "<link rel=\"stylesheet\" href=\"style.css\">";
         output_string oc "</head><body><h1>Documentation</h1>";
         output_string
           oc
           "<div class=\"controls\"><label for=\"search\">Search</label><input \
            id=\"search\" type=\"search\" placeholder=\"Filter docs...\"><button \
            id=\"toggle-theme\" type=\"button\">Dark mode</button></div>";
         List.iter
           (fun (loc, doc, sigstr) ->
              let doc_attr =
                doc |> String.split_on_char '\n' |> String.concat " " |> html_escape
              in
              output_string
                oc
                (Printf.sprintf
                   "<section data-signature=\"%s\" data-doc=\"%s\">"
                   (html_escape sigstr)
                   doc_attr);
              output_string
                oc
                (Printf.sprintf
                   "<p class=\"loc\">%s</p>"
                   (html_escape (Token.show_loc loc)));
              output_string
                oc
                (Printf.sprintf "<h2><code>%s</code></h2>" (html_escape sigstr));
              if String.length doc > 0
              then output_string oc (Printf.sprintf "<pre>%s</pre>" (html_escape doc));
              output_string oc "</section>")
           entries;
         output_string oc "<script src=\"script.js\"></script>";
         output_string oc "</body></html>")
      ~finally:(fun () -> close_out oc);
    Printf.sprintf "Wrote %s" doc_path
;;
