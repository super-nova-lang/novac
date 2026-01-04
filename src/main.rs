#![allow(dead_code, unused_imports)]

use anyhow::Result;
use clap::Parser as _;
use cli::Cli;

use analysis::{self, AnalysisResult};
use codegen;
use inkwell::context::Context;
use lexer::Lexer;
use lexer::token::Token;
use parser::{self, nodes::Node};

mod cli;

fn main() -> Result<()> {
    let cli = Cli::parse();

    match cli {
        Cli::Tokenize { files } => {
            for file in files {
                let source = std::fs::read_to_string(&file)?;
                let tokens = lex_step(&file, &source);
                for token in tokens {
                    println!("{:?}", token);
                }
            }
        }
        Cli::Parse { files } => {
            for file in files {
                let source = std::fs::read_to_string(&file)?;
                let tokens = lex_step(&file, &source);
                let ast = parse_step(tokens)?;

                // Print parse errors (if any) in human readable form and skip further analysis for this file.
                let mut had_parse_error = false;
                for node in &ast {
                    if let Node::Error(msg, span) = node {
                        println!(
                            "{}:{}:{} [error.parsing] {}",
                            span.file, span.line, span.column, msg
                        );
                        had_parse_error = true;
                    }
                }
                if had_parse_error {
                    continue;
                }

                let (errors, warnings) = analyze_step(ast.clone());

                println!("-- AST --\n{:#?}", ast);
                if !errors.is_empty() {
                    println!("-- Analysis Errors --");
                    for err in errors {
                        match err {
                            AnalysisResult::Error(e) => match e {
                                analysis::AnalysisError::UndefinedVariable(name, suggestions) => {
                                    println!(
                                        "{}:0:0 [error.analysis.undefined_variable] Undefined variable '{}' (suggestions: {})",
                                        file,
                                        name,
                                        suggestions.join(", ")
                                    );
                                }
                                analysis::AnalysisError::TypeMismatch(a, b) => {
                                    println!(
                                        "{}:0:0 [error.analysis.type_mismatch] Type mismatch: expected {:?}, found {:?}",
                                        file, a, b
                                    );
                                }
                                analysis::AnalysisError::DuplicateDeclaration(name, (r, c)) => {
                                    println!(
                                        "{}:{}:{} [error.analysis.duplicate_declaration] Duplicate declaration '{}'",
                                        file, r, c, name
                                    );
                                }
                                analysis::AnalysisError::InvalidOperation(msg) => {
                                    println!("{}:0:0 [error.analysis] {}", file, msg);
                                }
                                analysis::AnalysisError::MissingReturnType(name) => {
                                    println!(
                                        "{}:0:0 [error.analysis.missing_return] Missing return in function '{}'",
                                        file, name
                                    );
                                }
                            },
                            AnalysisResult::Warning(_) => {}
                        }
                    }
                }
                if !warnings.is_empty() {
                    println!("-- Analysis Warnings --");
                    for warn in warnings {
                        match warn {
                            AnalysisResult::Warning(w) => match w {
                                analysis::AnalysisWarning::UnusedVariable(name) => {
                                    println!(
                                        "{}:0:0 [warning.analysis.unused_variable] Unused variable '{}'",
                                        file, name
                                    );
                                }
                                analysis::AnalysisWarning::ShadowedVariable(name) => {
                                    println!(
                                        "{}:0:0 [warning.analysis.shadowed_variable] Shadowed variable '{}'",
                                        file, name
                                    );
                                }
                            },
                            AnalysisResult::Error(_) => {}
                        }
                    }
                }
            }
        }
        Cli::Codegen { files } => {
            for file in files {
                let source = std::fs::read_to_string(&file)?;
                let tokens = lex_step(&file, &source);
                let ast = parse_step(tokens)?;

                // Print parse errors
                let mut had_parse_error = false;
                for node in &ast {
                    if let Node::Error(msg, span) = node {
                        println!(
                            "{}:{}:{} [error.parsing] {}",
                            span.file, span.line, span.column, msg
                        );
                        had_parse_error = true;
                    }
                }
                if had_parse_error {
                    continue;
                }

                let (errors, warnings) = analyze_step(ast.clone());

                if !errors.is_empty() {
                    println!("-- Analysis Errors --");
                    for err in errors {
                        match err {
                            AnalysisResult::Error(e) => match e {
                                analysis::AnalysisError::UndefinedVariable(name, suggestions) => {
                                    println!(
                                        "{}:0:0 [error.analysis.undefined_variable] Undefined variable '{}' (suggestions: {})",
                                        file,
                                        name,
                                        suggestions.join(", ")
                                    );
                                }
                                analysis::AnalysisError::TypeMismatch(a, b) => {
                                    println!(
                                        "{}:0:0 [error.analysis.type_mismatch] Type mismatch: expected {:?}, found {:?}",
                                        file, a, b
                                    );
                                }
                                analysis::AnalysisError::DuplicateDeclaration(name, (r, c)) => {
                                    println!(
                                        "{}:{}:{} [error.analysis.duplicate_declaration] Duplicate declaration '{}'",
                                        file, r, c, name
                                    );
                                }
                                analysis::AnalysisError::InvalidOperation(msg) => {
                                    println!("{}:0:0 [error.analysis] {}", file, msg);
                                }
                                analysis::AnalysisError::MissingReturnType(name) => {
                                    println!(
                                        "{}:0:0 [error.analysis.missing_return] Missing return in function '{}'",
                                        file, name
                                    );
                                }
                            },
                            AnalysisResult::Warning(_) => {}
                        }
                    }
                    continue;
                }
                if !warnings.is_empty() {
                    println!("-- Analysis Warnings --");
                    for warn in warnings {
                        match warn {
                            AnalysisResult::Warning(w) => match w {
                                analysis::AnalysisWarning::UnusedVariable(name) => {
                                    println!(
                                        "{}:0:0 [warning.analysis.unused_variable] Unused variable '{}'",
                                        file, name
                                    );
                                }
                                analysis::AnalysisWarning::ShadowedVariable(name) => {
                                    println!(
                                        "{}:0:0 [warning.analysis.shadowed_variable] Shadowed variable '{}'",
                                        file, name
                                    );
                                }
                            },
                            AnalysisResult::Error(_) => {}
                        }
                    }
                }

                let context = Context::create();
                let codegen = codegen::Codegen::new(&context, &file);
                match codegen.compile(&ast) {
                    Ok(module) => {
                        println!("{}", module.print_to_string().to_string());
                    }
                    Err(e) => {
                        println!("Codegen failed for {}: {}", file, e);
                    }
                }
            }
        }
        Cli::Compile { files } => {
            for file in files {
                let source = std::fs::read_to_string(&file)?;
                let tokens = lex_step(&file, &source);
                let ast = parse_step(tokens)?;

                // Print parse errors
                let mut had_parse_error = false;
                for node in &ast {
                    if let Node::Error(msg, span) = node {
                        println!(
                            "{}:{}:{} [error.parsing] {}",
                            span.file, span.line, span.column, msg
                        );
                        had_parse_error = true;
                    }
                }
                if had_parse_error {
                    continue;
                }

                let (errors, warnings) = analyze_step(ast.clone());

                if !errors.is_empty() {
                    println!("-- Analysis Errors --");
                    for err in errors {
                        match err {
                            AnalysisResult::Error(e) => match e {
                                analysis::AnalysisError::UndefinedVariable(name, suggestions) => {
                                    println!(
                                        "{}:0:0 [error.analysis.undefined_variable] Undefined variable '{}' (suggestions: {})",
                                        file,
                                        name,
                                        suggestions.join(", ")
                                    );
                                }
                                analysis::AnalysisError::TypeMismatch(a, b) => {
                                    println!(
                                        "{}:0:0 [error.analysis.type_mismatch] Type mismatch: expected {:?}, found {:?}",
                                        file, a, b
                                    );
                                }
                                analysis::AnalysisError::DuplicateDeclaration(name, (r, c)) => {
                                    println!(
                                        "{}:{}:{} [error.analysis.duplicate_declaration] Duplicate declaration '{}'",
                                        file, r, c, name
                                    );
                                }
                                analysis::AnalysisError::InvalidOperation(msg) => {
                                    println!("{}:0:0 [error.analysis] {}", file, msg);
                                }
                                analysis::AnalysisError::MissingReturnType(name) => {
                                    println!(
                                        "{}:0:0 [error.analysis.missing_return] Missing return in function '{}'",
                                        file, name
                                    );
                                }
                            },
                            AnalysisResult::Warning(_) => {}
                        }
                    }
                    continue;
                }
                if !warnings.is_empty() {
                    println!("-- Analysis Warnings --");
                    for warn in warnings {
                        match warn {
                            AnalysisResult::Warning(w) => match w {
                                analysis::AnalysisWarning::UnusedVariable(name) => {
                                    println!(
                                        "{}:0:0 [warning.analysis.unused_variable] Unused variable '{}'",
                                        file, name
                                    );
                                }
                                analysis::AnalysisWarning::ShadowedVariable(name) => {
                                    println!(
                                        "{}:0:0 [warning.analysis.shadowed_variable] Shadowed variable '{}'",
                                        file, name
                                    );
                                }
                            },
                            AnalysisResult::Error(_) => {}
                        }
                    }
                }

                let context = Context::create();
                let codegen = codegen::Codegen::new(&context, &file);
                match codegen.compile(&ast) {
                    Ok(module) => {
                        let ir = module.print_to_string().to_string();
                        // Place LLVM and linked artifacts under ./build
                        let out_base = std::path::Path::new("build");
                        let emit_dir = out_base.join("emit");
                        let debug_dir = out_base.join("debug");
                        std::fs::create_dir_all(&emit_dir)?;
                        std::fs::create_dir_all(&debug_dir)?;
                        let stem = std::path::Path::new(&file)
                            .file_stem()
                            .and_then(|s| s.to_str())
                            .unwrap_or("output");
                        let ll_path = emit_dir.join(format!("{}.ll", stem));
                        std::fs::write(ll_path.to_str().unwrap(), ir)?;

                        // Attempt to link using clang. Output placed in build/debug with .out suffix.
                        let out_path = debug_dir.join(format!("{}.out", stem));
                        match std::process::Command::new("clang")
                            .arg(ll_path.to_str().unwrap())
                            .arg("-o")
                            .arg(out_path.to_str().unwrap())
                            .status()
                        {
                            Ok(s) if s.success() => {
                                println!("Linked executable: {}", out_path.display());
                            }
                            Ok(s) => {
                                println!("Linker failed for {}: status {}", file, s);
                            }
                            Err(e) => {
                                println!("Failed to invoke linker for {}: {}", file, e);
                            }
                        }
                    }
                    Err(e) => {
                        println!("Codegen failed for {}: {}", file, e);
                    }
                }
            }
        }
        Cli::Run { files } => {
            for file in files {
                let source = std::fs::read_to_string(&file)?;
                let tokens = lex_step(&file, &source);
                let ast = parse_step(tokens)?;

                // Print parse errors
                let mut had_parse_error = false;
                for node in &ast {
                    if let Node::Error(msg, span) = node {
                        println!(
                            "{}:{}:{} [error.parsing] {}",
                            span.file, span.line, span.column, msg
                        );
                        had_parse_error = true;
                    }
                }
                if had_parse_error {
                    continue;
                }

                let (errors, _warnings) = analyze_step(ast.clone());

                if !errors.is_empty() {
                    println!("-- Analysis Errors --");
                    continue;
                }

                let context = Context::create();
                let codegen = codegen::Codegen::new(&context, &file);
                match codegen.compile(&ast) {
                    Ok(module) => {
                        let ir = module.print_to_string().to_string();
                        // Place LLVM and linked artifacts under ./build
                        let out_base = std::path::Path::new("build");
                        let emit_dir = out_base.join("emit");
                        let debug_dir = out_base.join("debug");
                        std::fs::create_dir_all(&emit_dir)?;
                        std::fs::create_dir_all(&debug_dir)?;
                        let stem = std::path::Path::new(&file)
                            .file_stem()
                            .and_then(|s| s.to_str())
                            .unwrap_or("output");
                        let ll_path = emit_dir.join(format!("{}.ll", stem));
                        std::fs::write(ll_path.to_str().unwrap(), ir)?;

                        // Attempt to link using clang. Output placed in build/debug with .out suffix.
                        let out_path = debug_dir.join(format!("{}.out", stem));
                        match std::process::Command::new("clang")
                            .arg(ll_path.to_str().unwrap())
                            .arg("-o")
                            .arg(out_path.to_str().unwrap())
                            .status()
                        {
                            Ok(s) if s.success() => {
                                println!("Linked executable: {}", out_path.display());
                                // Run the produced executable and forward its exit status
                                match std::process::Command::new(out_path.to_str().unwrap())
                                    .status()
                                {
                                    Ok(rs) => {
                                        println!("Execution finished: {}", rs);
                                    }
                                    Err(e) => {
                                        println!("Failed to execute {}: {}", out_path.display(), e);
                                    }
                                }
                            }
                            Ok(s) => {
                                println!("Linker failed for {}: status {}", file, s);
                            }
                            Err(e) => {
                                println!("Failed to invoke linker for {}: {}", file, e);
                            }
                        }
                    }
                    Err(e) => {
                        println!("Codegen failed for {}: {}", file, e);
                    }
                }
            }
        }
        Cli::Clean => {
            let targets = ["build/emit", "build/debug"];
            for t in &targets {
                let p = std::path::Path::new(t);
                if p.exists() {
                    for entry in std::fs::read_dir(p)? {
                        let entry = entry?;
                        let path = entry.path();
                        if path.is_dir() {
                            std::fs::remove_dir_all(&path)?;
                        } else {
                            std::fs::remove_file(&path)?;
                        }
                    }
                }
            }
            println!("Cleaned build/emit and build/debug");
        }
        _ => println!("Implement CLI command: {}", cli),
    }

    Ok(())
}

fn lex_step(file: &str, source: &str) -> Vec<Token> {
    Lexer::new(file, source).tokenize()
}

fn parse_step(tokens: Vec<Token>) -> Result<Vec<Node>> {
    parser::parse(tokens).map_err(|e| anyhow::anyhow!("parse error: {:?}", e))
}

fn analyze_step(nodes: Vec<Node>) -> (Vec<AnalysisResult>, Vec<AnalysisResult>) {
    analysis::analyze(nodes)
}

// TODO: node type
fn optimize_step(_nodes: usize) {
    todo!()
}

// TODO: node type
fn codegen_step(_nodes: usize) {
    todo!()
}
