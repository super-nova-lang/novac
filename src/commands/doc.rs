use anyhow::Result;
use parser::nodes::{DeclStmt, Node, Statement, Type, Expression, VariantBody};
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

    // Generate HTML pages for each module
    for (module_path, entries) in &all_docs {
        let file_path = if module_path.is_empty() {
            doc_dir.join("index.html")
        } else {
            doc_dir.join(format!("{}.html", module_path.replace("::", "_")))
        };

        let html = generate_module_html(module_path, entries);
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
                extract_decl_docs(decl, all_docs, current_module.clone());
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
            doc,
            name,
            body,
            ..
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

fn format_decl_signature(name: &str, generics: &[String], expr: &Option<Box<Expression>>) -> String {
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
                            let field_types: Vec<String> = fields
                                .iter()
                                .map(|(_, t, _)| format_type(t))
                                .collect();
                            format!("{}({})", v_name, field_types.join(", "))
                        }
                        Some(VariantBody::TypeBody(t)) => {
                            format!("{}({})", v_name, format_type(t))
                        }
                        None => v_name.clone(),
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
        None => format!("let {} :: ...", name),
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

fn generate_module_html(module_path: &str, entries: &[DocEntry]) -> String {
    let title = if module_path.is_empty() {
        "Documentation".to_string()
    } else {
        format!("{} - Documentation", module_path)
    };

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
  <style>
    body {{
      font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, Oxygen, Ubuntu, sans-serif;
      line-height: 1.6;
      color: #333;
      max-width: 900px;
      margin: 0 auto;
      padding: 20px;
      background: #f5f5f5;
    }}
    h1 {{
      color: #222;
      border-bottom: 3px solid #007acc;
      padding-bottom: 10px;
    }}
    .entry {{
      background: white;
      padding: 15px;
      margin: 15px 0;
      border-radius: 5px;
      border-left: 4px solid #007acc;
    }}
    .signature {{
      font-size: 14px;
      margin-bottom: 8px;
      overflow-x: auto;
    }}
    .signature code {{
      background: #f0f0f0;
      padding: 2px 6px;
      border-radius: 3px;
      font-family: "Courier New", monospace;
    }}
    p {{
      margin: 5px 0;
      color: #555;
    }}
  </style>
</head>
<body>
  <h1>{}</h1>
  {}
</body>
</html>"#,
        title, title, entries_html
    )
}

fn escape_html(s: &str) -> String {
    s.replace("&", "&amp;")
        .replace("<", "&lt;")
        .replace(">", "&gt;")
        .replace("\"", "&quot;")
        .replace("'", "&#39;")
}
