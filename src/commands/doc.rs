use anyhow::Result;
use parser::nodes::{DeclStmt, Node, Statement};

use crate::commands::common::{analyze_step, lex_step, parse_step, read_source_with_stdlib, report_parse_errors};

pub fn run(files: Vec<String>) -> Result<()> {
    for file in files {
        let source = read_source_with_stdlib(&file)?;
        let tokens = lex_step(&file, &source);
        let ast = parse_step(tokens)?;

        if report_parse_errors(&ast) {
            continue;
        }

        let (_errors, _warnings) = analyze_step(ast.clone());

        println!("# Documentation for {}\n", file);
        extract_and_print_docs(&ast);
    }

    Ok(())
}

fn extract_and_print_docs(nodes: &[Node]) {
    for node in nodes {
        match node {
            Node::Statement(Statement::Decl(decl)) => {
                print_decl_doc(decl);
            }
            _ => {}
        }
    }
}

fn print_decl_doc(decl: &DeclStmt) {
    match decl {
        DeclStmt::Decl { doc, name, .. } => {
            if let Some(doc_text) = doc {
                println!("**{}**", name);
                println!("{}\n", doc_text);
            }
        }
        DeclStmt::CurryDecl { doc, name, .. } => {
            if let Some(doc_text) = doc {
                println!("**{}**", name);
                println!("{}\n", doc_text);
            }
        }
        DeclStmt::ImportDecl { doc, name, .. } => {
            if let Some(doc_text) = doc {
                println!("**{}**", name);
                println!("{}\n", doc_text);
            }
        }
        DeclStmt::ModuleDecl { doc, name, body, .. } => {
            if let Some(doc_text) = doc {
                println!("## Module {}", name);
                println!("{}\n", doc_text);
            } else {
                println!("## Module {}\n", name);
            }
            extract_and_print_docs(body);
        }
        DeclStmt::ExportStmt(_) => {}
    }
}
