use anyhow::Result;
use crate::parser::nodes::{DeclStmt, Expression, Node, Statement, Type, VariantBody};
use std::collections::HashMap;
use std::path::Path;

use crate::commands::common::{
    analyze_step, lex_step, parse_step, read_source_with_stdlib, report_parse_errors,
};

#[derive(Clone)]
struct DocEntry {
    name: String,
    doc: Option<String>,
    signature: String,
    file_location: String,
    source_file: String,
}

pub fn run(files: Vec<String>) -> Result<()> {
    let doc_dir = Path::new("build/doc");
    std::fs::create_dir_all(doc_dir)?;

    let mut all_entries: Vec<DocEntry> = Vec::new();
    let mut all_return_types = std::collections::HashMap::new();

    for file in files {
        let source = read_source_with_stdlib(&file)?;
        let tokens = lex_step(&file, &source);
        let ast = parse_step(tokens)?;

        if report_parse_errors(&ast) {
            continue;
        }

        let (_errors, _warnings, return_types) = analyze_step(ast.clone());
        all_return_types.extend(return_types);

        extract_docs_flat(
            &ast,
            &mut all_entries,
            String::new(),
            &file,
            &all_return_types,
        );
    }

    // Copy CSS file
    let css_content = std::fs::read_to_string("assets/style.css")?;
    std::fs::write(doc_dir.join("style.css"), css_content)?;
    println!("Generated: {}/style.css", doc_dir.display());

    // Copy JavaScript file
    let js_content = std::fs::read_to_string("assets/script.js")?;
    std::fs::write(doc_dir.join("script.js"), js_content)?;
    println!("Generated: {}/script.js", doc_dir.display());

    // Separate stdlib and other files based on file_location
    let mut user_entries: Vec<DocEntry> = Vec::new();
    let mut stdlib_entries: Vec<DocEntry> = Vec::new();

    for entry in all_entries.iter() {
        let location_file = entry.file_location.split(':').next().unwrap_or("");
        if location_file.starts_with("stdlib/") {
            stdlib_entries.push(entry.clone());
        } else {
            user_entries.push(entry.clone());
        }
    }

    // Group user files by source file
    let mut user_files: HashMap<String, Vec<DocEntry>> = HashMap::new();
    for entry in user_entries.iter() {
        user_files
            .entry(entry.source_file.clone())
            .or_insert_with(Vec::new)
            .push(entry.clone());
    }

    // Group stdlib files by location file
    let mut stdlib_files: HashMap<String, Vec<DocEntry>> = HashMap::new();
    for entry in stdlib_entries.iter() {
        let location_file = entry
            .file_location
            .split(':')
            .next()
            .unwrap_or("")
            .to_string();
        stdlib_files
            .entry(location_file)
            .or_insert_with(Vec::new)
            .push(entry.clone());
    }

    // Create sorted file lists
    let mut sorted_user_files: Vec<_> = user_files.keys().collect();
    sorted_user_files.sort();

    let mut sorted_stdlib_files: Vec<_> = stdlib_files.keys().collect();
    sorted_stdlib_files.sort();

    // Generate HTML pages for user files
    for file_name in &sorted_user_files {
        let entries = &user_files[*file_name];
        let file_html_name = file_name.replace("/", "_").replace(".", "_") + ".html";
        let html = generate_file_html(
            file_name,
            entries,
            &sorted_user_files,
            &sorted_stdlib_files,
            false,
        );
        let file_path = doc_dir.join(&file_html_name);
        std::fs::write(&file_path, html)?;
        println!("Generated: {}", file_path.display());
    }

    // Generate HTML pages for stdlib files
    for file_name in &sorted_stdlib_files {
        let entries = &stdlib_files[*file_name];
        let file_html_name = file_name.replace("/", "_").replace(".", "_") + ".html";
        let html = generate_file_html(
            file_name,
            entries,
            &sorted_user_files,
            &sorted_stdlib_files,
            true,
        );
        let file_path = doc_dir.join(&file_html_name);
        std::fs::write(&file_path, html)?;
        println!("Generated: {}", file_path.display());
    }

    // Generate stdlib index page if there are stdlib files
    if !sorted_stdlib_files.is_empty() {
        let stdlib_index_html = generate_stdlib_index_html(&stdlib_entries, &sorted_stdlib_files);
        let file_path = doc_dir.join("stdlib.html");
        std::fs::write(&file_path, stdlib_index_html)?;
        println!("Generated: {}", file_path.display());
    }

    // Generate main index page
    let index_html = generate_index_html(
        &all_entries,
        &sorted_user_files,
        &sorted_stdlib_files,
        !sorted_stdlib_files.is_empty(),
    );
    let file_path = doc_dir.join("index.html");
    std::fs::write(&file_path, index_html)?;
    println!("Generated: {}", file_path.display());

    Ok(())
}

fn extract_docs_flat(
    nodes: &[Node],
    entries: &mut Vec<DocEntry>,
    current_module: String,
    file: &str,
    return_types: &std::collections::HashMap<String, crate::parser::nodes::Type>,
) {
    for (idx, node) in nodes.iter().enumerate() {
        match node {
            Node::Statement(Statement::Decl(decl)) => {
                extract_decl_docs_flat(
                    &decl,
                    entries,
                    current_module.clone(),
                    file,
                    idx as u32 + 1,
                    return_types,
                );
            }
            _ => {}
        }
    }
}

fn extract_with_block(expr: &Option<Box<Expression>>) -> Option<&crate::parser::nodes::WithBlock> {
    if let Some(e) = expr {
        match e.as_ref() {
            Expression::StructExpr(_, with_block) => with_block.as_ref(),
            Expression::EnumExpr(_, with_block) => with_block.as_ref(),
            _ => None,
        }
    } else {
        None
    }
}

fn extract_with_block_methods(
    with_block: &crate::parser::nodes::WithBlock,
    entries: &mut Vec<DocEntry>,
    struct_name: String,
    file: &str,
    return_types: &std::collections::HashMap<String, crate::parser::nodes::Type>,
) {
    for (idx, node) in with_block.iter().enumerate() {
        match node {
            Node::Statement(Statement::Decl(DeclStmt::Decl { doc, name, .. })) => {
                let return_type = return_types
                    .get(name)
                    .map(|t| format_type(t))
                    .unwrap_or_else(|| "()".to_string());
                entries.push(DocEntry {
                    name: name.clone(),
                    doc: doc.clone(),
                    signature: format!("{}({}) => {}", name, struct_name, return_type),
                    file_location: format!("{}:{}:1", file, idx as u32 + 1),
                    source_file: file.to_string(),
                });
            }
            _ => {}
        }
    }
}

fn extract_decl_docs_flat(
    decl: &DeclStmt,
    entries: &mut Vec<DocEntry>,
    current_module: String,
    file: &str,
    line_num: u32,
    return_types: &std::collections::HashMap<String, crate::parser::nodes::Type>,
) {
    match decl {
        DeclStmt::Decl {
            doc,
            name,
            generics,
            body: (_, expr),
            ..
        } => {
            let signature = format_decl_signature(name, generics, expr);
            entries.push(DocEntry {
                name: name.clone(),
                doc: doc.clone(),
                signature,
                file_location: format!("{}:{}:1", file, line_num),
                source_file: file.to_string(),
            });

            // Extract methods from with block if this is a struct or enum
            if let Some(with_block) = extract_with_block(expr) {
                extract_with_block_methods(with_block, entries, name.clone(), file, return_types);
            }
        }
        DeclStmt::CurryDecl { doc, name, .. } => {
            entries.push(DocEntry {
                name: name.clone(),
                doc: doc.clone(),
                signature: format!("let {} :: ...", name),
                file_location: format!("{}:{}:1", file, line_num),
                source_file: file.to_string(),
            });
        }
        DeclStmt::ImportDecl { .. } => {
            // Skip imports in documentation
        }
        DeclStmt::ModuleDecl {
            doc, name, body, ..
        } => {
            let new_module = if current_module.is_empty() {
                format!("{}", name)
            } else {
                format!("{}.{}", current_module, name)
            };

            if let Some(doc_text) = doc {
                entries.push(DocEntry {
                    name: name.clone(),
                    doc: Some(doc_text.clone()),
                    signature: format!("module {}", name),
                    file_location: format!("{}:{}:1", file, line_num),
                    source_file: file.to_string(),
                });
            }

            extract_docs_flat(body, entries, new_module, file, return_types);
        }
        DeclStmt::ExportStmt(_) => {}
    }
}

fn format_decl_signature(
    name: &str,
    generics: &[String],
    expr: &Option<Box<Expression>>,
) -> String {
    if generics.is_empty() {
        format_expr_signature(name, expr)
    } else {
        let type_params = generics.join(", ");
        format_expr_signature(&format!("{}[{}]", name, type_params), expr)
    }
}

fn format_expr_signature(name: &str, expr: &Option<Box<Expression>>) -> String {
    match expr {
        Some(e) => match e.as_ref() {
            Expression::EnumExpr(variants, _) => {
                let variant_strs: Vec<String> = variants
                    .iter()
                    .map(|(v_name, body)| match body {
                        Some(VariantBody::StructBody(fields)) => {
                            let field_types: Vec<String> =
                                fields.iter().map(|(_, t, _)| format_type(t)).collect();
                            format!("{}({})", v_name, field_types.join(", "))
                        }
                        Some(VariantBody::TypeBody(t)) => {
                            format!("{}({})", v_name, format_type(t))
                        }
                        _ => v_name.clone(),
                    })
                    .collect();
                format!("{} => enum {{ {} }}", name, variant_strs.join(", "))
            }
            Expression::StructExpr(fields, _) => {
                let field_strs: Vec<String> = fields
                    .iter()
                    .map(|(fname, t, _)| format!("{}: {}", fname, format_type(t)))
                    .collect();
                format!("{} => struct {{ {} }}", name, field_strs.join(", "))
            }
            _ => format!("let {} :: ...", name),
        },
        _ => format!("let {} :: ...", name),
    }
}

fn format_type(t: &Type) -> String {
    match t {
        Type::User(name) => name.clone(),
        Type::Generic(name, args) => {
            let arg_strs: Vec<String> = args.iter().map(format_type).collect();
            format!("{}[{}]", name, arg_strs.join(", "))
        }
        Type::TypeVar(name) => name.clone(),
        Type::Builtin(name) => name.clone(),
        Type::UnitTyp => "()".to_string(),
        Type::ListTyp(inner) => format!("[{}]", format_type(inner)),
    }
}

fn extract_first_param_type(signature: &str) -> Option<String> {
    // Parse signature like "func(Type1, Type2) => Type3"
    // Extract "Type1"
    if let Some(paren_idx) = signature.find('(') {
        if let Some(comma_or_close) = signature[paren_idx + 1..].find(|c| c == ',' || c == ')') {
            let first_param = signature[paren_idx + 1..paren_idx + 1 + comma_or_close].trim();
            if !first_param.is_empty() {
                return Some(first_param.to_string());
            }
        }
    }
    None
}

fn extract_return_type(signature: &str) -> String {
    // Parse signature like "func(Type1, Type2) => Type3"
    // Extract "Type3"
    if let Some(arrow_idx) = signature.find("=>") {
        signature[arrow_idx + 2..].trim().to_string()
    } else {
        signature.to_string()
    }
}

fn generate_file_html(
    file_name: &str,
    entries: &[DocEntry],
    user_files: &[&String],
    stdlib_files: &[&String],
    is_stdlib: bool,
) -> String {
    // Generate navigation sidebar
    let nav_html = generate_nav(file_name, user_files, stdlib_files, is_stdlib);

    // Separate type definitions from functions, and group functions with their types
    let mut type_entries = Vec::new();
    let mut function_entries = Vec::new();

    for e in entries.iter() {
        if !e.signature.starts_with("module ") {
            if e.signature.contains(" => struct ") || e.signature.contains(" => enum ") {
                type_entries.push(e.clone());
            } else {
                function_entries.push(e.clone());
            }
        }
    }

    // Generate HTML for types with their methods
    let mut entries_html = String::new();
    let mut displayed_methods = std::collections::HashSet::new();

    for type_entry in &type_entries {
        let doc_text = type_entry
            .doc
            .as_ref()
            .map(|d| format!("<pre>{}</pre>", escape_html(d)))
            .unwrap_or_default();

        entries_html.push_str(&format!(
            r#"        <section data-signature="{}" data-doc="{}">
            <p class="loc">{}</p>
            <h2><code>{}</code></h2>
            {}
"#,
            escape_html(&type_entry.signature),
            type_entry
                .doc
                .as_ref()
                .map(|d| escape_html(d))
                .unwrap_or_default(),
            escape_html(&type_entry.file_location),
            escape_html(&type_entry.signature),
            doc_text
        ));

        // Find methods that use this type
        let type_name = type_entry.name.clone();
        let methods: Vec<_> = function_entries
            .iter()
            .filter(|f| extract_first_param_type(&f.signature) == Some(type_name.clone()))
            .collect();

        if !methods.is_empty() {
            entries_html.push_str("            <div class=\"methods\">\n");
            for method in &methods {
                entries_html.push_str(&format!(
                    r#"                <div class="method">
                    <p class="method-name">-> <code>{}</code></p>
                </div>
"#,
                    escape_html(&method.signature)
                ));
                displayed_methods.insert(method.signature.clone());
            }
            entries_html.push_str("            </div>\n");
        }

        entries_html.push_str("        </section>\n");
    }

    // Add remaining functions (not methods of any type)
    for func_entry in &function_entries {
        if !displayed_methods.contains(&func_entry.signature) {
            let doc_text = func_entry
                .doc
                .as_ref()
                .map(|d| format!("<pre>{}</pre>", escape_html(d)))
                .unwrap_or_default();
            entries_html.push_str(&format!(
                r#"        <section data-signature="{}" data-doc="{}">
            <p class="loc">{}</p>
            <h2><code>{}</code></h2>
            {}
        </section>
"#,
                escape_html(&func_entry.signature),
                func_entry
                    .doc
                    .as_ref()
                    .map(|d| escape_html(d))
                    .unwrap_or_default(),
                escape_html(&func_entry.file_location),
                escape_html(&func_entry.signature),
                doc_text
            ));
        }
    }

    format!(
        r#"<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="utf-8">
    <title>{} - Documentation</title>
    <link rel="stylesheet" href="style.css">
</head>
<body>
    <div class="container">
        <nav class="sidebar">
            {}
        </nav>
        <main>
            <h1>{}</h1>
            
            <div class="controls">
                <label for="search">Search</label>
                <input id="search" type="search" placeholder="Filter by name, type, or file...">
                <button id="toggle-theme" type="button">Dark mode</button>
            </div>

            {}

            <script src="script.js"></script>
        </main>
    </div>
</body>
</html>"#,
        file_name, nav_html, file_name, entries_html
    )
}

fn generate_nav(
    current_file: &str,
    user_files: &[&String],
    stdlib_files: &[&String],
    is_stdlib: bool,
) -> String {
    let mut nav = String::from("<ul>\n");

    // Add user files section
    if !user_files.is_empty() {
        nav.push_str("    <li><strong>Files</strong><ul>\n");
        for file_name in user_files {
            let file_html_name = file_name.replace("/", "_").replace(".", "_") + ".html";
            let is_current = *file_name == current_file && !is_stdlib;
            let class = if is_current { " class=\"active\"" } else { "" };
            nav.push_str(&format!(
                "        <li><a href=\"{}\"{}>{}</a></li>\n",
                file_html_name,
                class,
                escape_html(file_name)
            ));
        }
        nav.push_str("    </ul></li>\n");
    }

    // Add stdlib section if it exists
    if !stdlib_files.is_empty() {
        nav.push_str("    <li><strong>Standard Library</strong><ul>\n");
        nav.push_str("        <li><a href=\"stdlib.html\">Overview</a></li>\n");
        for file_name in stdlib_files {
            let file_html_name = file_name.replace("/", "_").replace(".", "_") + ".html";
            let is_current = *file_name == current_file && is_stdlib;
            let class = if is_current { " class=\"active\"" } else { "" };
            nav.push_str(&format!(
                "        <li><a href=\"{}\"{}>{}</a></li>\n",
                file_html_name,
                class,
                escape_html(file_name)
            ));
        }
        nav.push_str("    </ul></li>\n");
    }

    nav.push_str("</ul>");
    nav
}

fn generate_stdlib_index_html(entries: &[DocEntry], stdlib_files: &[&String]) -> String {
    // Generate navigation sidebar
    let nav_html = generate_nav("", &[], stdlib_files, true);

    // Generate entries for stdlib index
    let entries_html = entries
        .iter()
        .filter(|e| !e.signature.starts_with("module "))
        .map(|e| {
            let doc_text = e
                .doc
                .as_ref()
                .map(|d| format!("<pre>{}</pre>", escape_html(d)))
                .unwrap_or_else(|| "<pre></pre>".to_string());
            format!(
                r#"        <section data-signature="{}" data-doc="{}">
            <p class="loc">{}</p>
            <h2><code>{}</code></h2>
            {}
        </section>"#,
                escape_html(&e.signature),
                e.doc.as_ref().map(|d| escape_html(d)).unwrap_or_default(),
                escape_html(&e.file_location),
                escape_html(&e.signature),
                doc_text
            )
        })
        .collect::<Vec<_>>()
        .join("\n");

    format!(
        r#"<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="utf-8">
    <title>Standard Library - Documentation</title>
    <link rel="stylesheet" href="style.css">
</head>
<body>
    <div class="container">
        <nav class="sidebar">
            {}
        </nav>
        <main>
            <h1>Standard Library</h1>
            
            <div class="controls">
                <label for="search">Search</label>
                <input id="search" type="search" placeholder="Filter by name, type, or file...">
                <button id="toggle-theme" type="button">Dark mode</button>
            </div>

            {}

            <script src="script.js"></script>
        </main>
    </div>
</body>
</html>"#,
        nav_html, entries_html
    )
}

fn generate_index_html(
    _entries: &[DocEntry],
    user_files: &[&String],
    stdlib_files: &[&String],
    _has_stdlib: bool,
) -> String {
    // Generate navigation sidebar
    let nav_html = generate_nav("", user_files, stdlib_files, false);

    // Generate overview content with links to files
    let mut overview = String::new();

    if !user_files.is_empty() {
        overview.push_str("        <section>\n            <h2>Files</h2>\n            <ul>\n");
        for file_name in user_files {
            let file_html_name = file_name.replace("/", "_").replace(".", "_") + ".html";
            overview.push_str(&format!(
                "                <li><a href=\"{}\">{}</a></li>\n",
                file_html_name,
                escape_html(file_name)
            ));
        }
        overview.push_str("            </ul>\n        </section>\n");
    }

    if !stdlib_files.is_empty() {
        overview.push_str(
            "        <section>\n            <h2>Standard Library</h2>\n            <ul>\n",
        );
        overview.push_str("                <li><a href=\"stdlib.html\">Overview</a></li>\n");
        for file_name in stdlib_files {
            let file_html_name = file_name.replace("/", "_").replace(".", "_") + ".html";
            overview.push_str(&format!(
                "                <li><a href=\"{}\">{}</a></li>\n",
                file_html_name,
                escape_html(file_name)
            ));
        }
        overview.push_str("            </ul>\n        </section>\n");
    }

    format!(
        r#"<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="utf-8">
    <title>Nova Documentation</title>
    <link rel="stylesheet" href="style.css">
</head>
<body>
    <div class="container">
        <nav class="sidebar">
            {}
        </nav>
        <main>
            <h1>Nova Documentation</h1>
            <p>Select a file from the navigation to view its documentation.</p>
            
            <div class="controls">
                <button id="toggle-theme" type="button">Dark mode</button>
            </div>

            {}

            <script src="script.js"></script>
        </main>
    </div>
</body>
</html>"#,
        nav_html, overview
    )
}

fn escape_html(s: &str) -> String {
    s.replace("&", "&amp;")
        .replace("<", "&lt;")
        .replace(">", "&gt;")
        .replace("\"", "&quot;")
        .replace("'", "&#39;")
}
