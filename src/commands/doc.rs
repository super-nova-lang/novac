use anyhow::Result;
use parser::nodes::{DeclStmt, Expression, Node, Statement, Type, VariantBody};
use std::collections::HashMap;
use std::path::Path;

use crate::commands::common::{
    analyze_step, lex_step, parse_step, read_source_with_stdlib, report_parse_errors,
};

struct DocEntry {
    name: String,
    doc: Option<String>,
    signature: String,
}

pub fn run(files: Vec<String>) -> Result<()> {
    let doc_dir = Path::new("build/doc");
    std::fs::create_dir_all(doc_dir)?;

    let mut all_docs: HashMap<String, Vec<DocEntry>> = HashMap::new();

    for file in files {
        let source = read_source_with_stdlib(&file)?;
        let tokens = lex_step(&file, &source);
        let ast = parse_step(tokens)?;

        if report_parse_errors(&ast) {
            continue;
        }

        let (_errors, _warnings) = analyze_step(ast.clone());

        extract_docs(&ast, &mut all_docs, String::new());
    }

    // Write CSS file
    std::fs::write(doc_dir.join("style.css"), generate_css())?;
    println!("Generated: {}/style.css", doc_dir.display());

    // Write JavaScript file
    std::fs::write(doc_dir.join("script.js"), generate_javascript())?;
    println!("Generated: {}/script.js", doc_dir.display());

    // Generate HTML pages for each module
    let mut sorted_modules: Vec<_> = all_docs.iter().collect();
    sorted_modules.sort_by_key(|(k, _)| k.as_str());

    for (module_path, entries) in sorted_modules {
        let file_path = if module_path.is_empty() {
            doc_dir.join("index.html")
        } else {
            doc_dir.join(format!("{}.html", module_path.replace("::", "_")))
        };

        let html = generate_module_html(module_path, entries, &all_docs);
        std::fs::write(&file_path, html)?;
        println!("Generated: {}", file_path.display());
    }

    Ok(())
}

fn extract_docs(
    nodes: &[Node],
    all_docs: &mut HashMap<String, Vec<DocEntry>>,
    current_module: String,
) {
    for node in nodes {
        match node {
            Node::Statement(Statement::Decl(decl)) => {
                extract_decl_docs(&decl, all_docs, current_module.clone());
            }
            _ => {}
        }
    }
}

fn extract_decl_docs(
    decl: &DeclStmt,
    all_docs: &mut HashMap<String, Vec<DocEntry>>,
    current_module: String,
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
            all_docs
                .entry(current_module)
                .or_insert_with(Vec::new)
                .push(DocEntry {
                    name: name.clone(),
                    doc: doc.clone(),
                    signature,
                });
        }
        DeclStmt::CurryDecl { doc, name, .. } => {
            all_docs
                .entry(current_module)
                .or_insert_with(Vec::new)
                .push(DocEntry {
                    name: name.clone(),
                    doc: doc.clone(),
                    signature: format!("let {} :: ...", name),
                });
        }
        DeclStmt::ImportDecl { doc, name, .. } => {
            all_docs
                .entry(current_module)
                .or_insert_with(Vec::new)
                .push(DocEntry {
                    name: name.clone(),
                    doc: doc.clone(),
                    signature: format!("import {}", name),
                });
        }
        DeclStmt::ModuleDecl {
            doc, name, body, ..
        } => {
            let new_module = if current_module.is_empty() {
                format!("std::{}", name)
            } else {
                format!("{}::{}", current_module, name)
            };

            if let Some(doc_text) = doc {
                all_docs
                    .entry(new_module.clone())
                    .or_insert_with(Vec::new)
                    .push(DocEntry {
                        name: name.clone(),
                        doc: Some(doc_text.clone()),
                        signature: format!("module {}", name),
                    });
            }

            extract_docs(body, all_docs, new_module);
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

fn generate_module_html(
    module_path: &str,
    entries: &[DocEntry],
    all_docs: &HashMap<String, Vec<DocEntry>>,
) -> String {
    let title = if module_path.is_empty() {
        "Documentation".to_string()
    } else {
        format!("{} - Documentation", module_path)
    };

    let breadcrumbs = if module_path.is_empty() {
        String::new()
    } else {
        let parts: Vec<&str> = module_path.split("::").collect();
        let mut breadcrumb_html = String::from("<nav class=\"breadcrumbs\">");
        breadcrumb_html.push_str("<a href=\"index.html\">Home</a>");
        let mut current_path = String::new();
        for (i, part) in parts.iter().enumerate() {
            if i == 0 {
                current_path.push_str(part);
            } else {
                current_path.push_str("::");
                current_path.push_str(part);
            }
            let file_name = current_path.replace("::", "_");
            if i == parts.len() - 1 {
                breadcrumb_html.push_str(&format!(" / <span>{}</span>", part));
            } else {
                breadcrumb_html.push_str(&format!(
                    " / <a href=\"{}.html\">{}</a>",
                    file_name, part
                ));
            }
        }
        breadcrumb_html.push_str("</nav>");
        breadcrumb_html
    };

    let nav_html = generate_navigation(all_docs, module_path);

    let entries_html = entries
        .iter()
        .filter(|e| !e.signature.starts_with("module "))
        .map(|e| {
            let doc_html = e
                .doc
                .as_ref()
                .map(|d| format!("<p>{}</p>", escape_html(d)))
                .unwrap_or_default();
            format!(
                r#"<div class="entry">
  <div class="signature"><code>{}</code></div>
  {}
</div>"#,
                escape_html(&e.signature),
                doc_html
            )
        })
        .collect::<Vec<_>>()
        .join("\n");

    format!(
        r#"<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>{}</title>
  <link rel="stylesheet" href="style.css">
</head>
<body>
  <div class="dark-mode-toggle" id="darkModeToggle">
    <input type="checkbox" id="darkModeCheckbox">
    <label for="darkModeCheckbox">🌙</label>
  </div>
  
  <div class="container">
    {}
    <aside class="sidebar">
      {}
    </aside>
    
    <main class="content">
      {}
      <h1>{}</h1>
      {}
    </main>
  </div>

  <script src="script.js"></script>
</body>
</html>"#,
        title, nav_html, generate_sidebar(all_docs), breadcrumbs, title, entries_html
    )
}

fn generate_navigation(all_docs: &HashMap<String, Vec<DocEntry>>, current_module: &str) -> String {
    let mut nav = String::from("<nav class=\"top-nav\">\n  <a href=\"index.html\" class=\"logo\">📚 Docs</a>\n");

    // Add module links (only show top-level std modules)
    for key in all_docs.keys() {
        if key.starts_with("std::") && !key[5..].contains("::") {
            let file_name = key.replace("::", "_");
            let display_name = key.trim_start_matches("std::");
            let class = if key == current_module {
                " class=\"active\""
            } else {
                ""
            };
            nav.push_str(&format!(
                "  <a href=\"{}.html\"{} class=\"nav-link\">{}</a>\n",
                file_name, class, display_name
            ));
        }
    }

    nav.push_str("</nav>");
    nav
}

fn generate_sidebar(all_docs: &HashMap<String, Vec<DocEntry>>) -> String {
    let mut sidebar = String::from("<div class=\"modules\">\n<h3>Modules</h3>\n<ul>\n");

    let mut sorted_keys: Vec<_> = all_docs.keys().collect();
    sorted_keys.sort();

    for key in sorted_keys {
        if !key.is_empty() {
            let file_name = key.replace("::", "_");
            sidebar.push_str(&format!(
                "  <li><a href=\"{}.html\">{}</a></li>\n",
                file_name, key
            ));
        }
    }

    sidebar.push_str("</ul>\n</div>");
    sidebar
}

fn escape_html(s: &str) -> String {
    s.replace("&", "&amp;")
        .replace("<", "&lt;")
        .replace(">", "&gt;")
        .replace("\"", "&quot;")
        .replace("'", "&#39;")
}

fn generate_css() -> String {
    r#"* {
  margin: 0;
  padding: 0;
  box-sizing: border-box;
}

:root {
  --bg-color: #ffffff;
  --text-color: #333333;
  --border-color: #e0e0e0;
  --code-bg: #f5f5f5;
  --accent-color: #007acc;
  --sidebar-bg: #f9f9f9;
  --entry-bg: #ffffff;
}

body.dark-mode {
  --bg-color: #1e1e1e;
  --text-color: #e0e0e0;
  --border-color: #333333;
  --code-bg: #2d2d2d;
  --sidebar-bg: #252525;
  --entry-bg: #2a2a2a;
}

body {
  font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, Oxygen, Ubuntu, sans-serif;
  line-height: 1.6;
  color: var(--text-color);
  background: var(--bg-color);
  transition: background-color 0.3s, color 0.3s;
}

.dark-mode-toggle {
  position: fixed;
  top: 20px;
  right: 20px;
  z-index: 100;
}

.dark-mode-toggle input {
  display: none;
}

.dark-mode-toggle label {
  cursor: pointer;
  font-size: 24px;
  user-select: none;
  transition: transform 0.3s;
}

.dark-mode-toggle label:hover {
  transform: scale(1.1);
}

.container {
  display: flex;
  max-width: 1200px;
  margin: 0 auto;
  gap: 30px;
  padding: 20px;
}

.top-nav {
  display: flex;
  align-items: center;
  gap: 20px;
  padding: 15px 20px;
  background: var(--sidebar-bg);
  border-bottom: 2px solid var(--accent-color);
  margin-bottom: 20px;
  flex-wrap: wrap;
}

.top-nav .logo {
  font-weight: bold;
  font-size: 18px;
  color: var(--accent-color);
  text-decoration: none;
  margin-right: 20px;
}

.nav-link {
  color: var(--text-color);
  text-decoration: none;
  padding: 5px 10px;
  border-radius: 3px;
  transition: background 0.2s;
}

.nav-link:hover,
.nav-link.active {
  background: var(--accent-color);
  color: white;
}

.breadcrumbs {
  display: block;
  margin-bottom: 20px;
  padding: 10px;
  background: var(--code-bg);
  border-radius: 3px;
  font-size: 14px;
}

.breadcrumbs a {
  color: var(--accent-color);
  text-decoration: none;
}

.breadcrumbs a:hover {
  text-decoration: underline;
}

.breadcrumbs span {
  color: var(--text-color);
}

.sidebar {
  flex: 0 0 250px;
}

.modules {
  background: var(--sidebar-bg);
  padding: 20px;
  border-radius: 5px;
  border: 1px solid var(--border-color);
  position: sticky;
  top: 20px;
}

.modules h3 {
  margin-bottom: 15px;
  color: var(--accent-color);
  font-size: 16px;
}

.modules ul {
  list-style: none;
}

.modules li {
  margin-bottom: 8px;
}

.modules a {
  color: var(--text-color);
  text-decoration: none;
  padding: 5px 0;
  display: block;
  border-left: 3px solid transparent;
  padding-left: 10px;
  transition: border-color 0.2s, color 0.2s;
}

.modules a:hover {
  border-left-color: var(--accent-color);
  color: var(--accent-color);
}

.content {
  flex: 1;
  min-width: 0;
}

h1 {
  color: var(--accent-color);
  border-bottom: 3px solid var(--accent-color);
  padding-bottom: 10px;
  margin-bottom: 30px;
}

.entry {
  background: var(--entry-bg);
  padding: 20px;
  margin: 20px 0;
  border-radius: 5px;
  border-left: 4px solid var(--accent-color);
  border: 1px solid var(--border-color);
  border-left: 4px solid var(--accent-color);
  transition: box-shadow 0.2s;
}

.entry:hover {
  box-shadow: 0 2px 8px rgba(0, 0, 0, 0.1);
}

.signature {
  font-size: 14px;
  margin-bottom: 12px;
  overflow-x: auto;
}

.signature code {
  background: var(--code-bg);
  padding: 8px 12px;
  border-radius: 3px;
  font-family: "Courier New", "Monaco", monospace;
  display: block;
  overflow-x: auto;
  border: 1px solid var(--border-color);
}

.entry p {
  margin: 10px 0;
  color: var(--text-color);
}

@media (max-width: 768px) {
  .container {
    flex-direction: column;
  }

  .sidebar {
    flex: 1;
    position: relative;
    top: auto;
  }

  .modules {
    position: relative;
    top: auto;
  }

  .top-nav {
    gap: 10px;
  }

  .dark-mode-toggle {
    position: static;
  }
}"#.to_string()
}

fn generate_javascript() -> String {
    "(function() {
  const DARK_MODE_KEY = 'novac-docs-dark-mode';
  const checkbox = document.getElementById('darkModeCheckbox');
  const htmlElement = document.documentElement;

  // Initialize dark mode from localStorage
  function initDarkMode() {
    const savedMode = localStorage.getItem(DARK_MODE_KEY);
    const prefersDark = window.matchMedia('(prefers-color-scheme: dark)').matches;
    const isDarkMode = savedMode !== null ? savedMode === 'true' : prefersDark;
    
    if (isDarkMode) {
      enableDarkMode();
    }
  }

  function enableDarkMode() {
    document.body.classList.add('dark-mode');
    checkbox.checked = true;
    localStorage.setItem(DARK_MODE_KEY, 'true');
  }

  function disableDarkMode() {
    document.body.classList.remove('dark-mode');
    checkbox.checked = false;
    localStorage.setItem(DARK_MODE_KEY, 'false');
  }

  // Event listener for toggle
  checkbox.addEventListener('change', function() {
    if (this.checked) {
      enableDarkMode();
    } else {
      disableDarkMode();
    }
  });

  // Initialize on load
  initDarkMode();

  // Smooth scroll for navigation
  document.querySelectorAll('a[href^=\"#\"]').forEach(anchor => {
    anchor.addEventListener('click', function(e) {
      e.preventDefault();
      const target = document.querySelector(this.getAttribute('href'));
      if (target) {
        target.scrollIntoView({ behavior: 'smooth' });
      }
    });
  });
})();".to_string()
}

