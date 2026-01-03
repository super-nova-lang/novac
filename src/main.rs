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
                let (errors, warnings) = analyze_step(ast.clone());

                println!("-- AST --\n{:#?}", ast);
                if !errors.is_empty() {
                    println!("-- Analysis Errors --");
                    for err in errors {
                        println!("{:?}", err);
                    }
                }
                if !warnings.is_empty() {
                    println!("-- Analysis Warnings --");
                    for warn in warnings {
                        println!("{:?}", warn);
                    }
                }
            }
        }
        Cli::Codegen { files } => {
            for file in files {
                let source = std::fs::read_to_string(&file)?;
                let tokens = lex_step(&file, &source);
                let ast = parse_step(tokens)?;
                let (errors, warnings) = analyze_step(ast.clone());

                if !errors.is_empty() {
                    println!("-- Analysis Errors --");
                    for err in errors {
                        println!("{:?}", err);
                    }
                    continue;
                }
                if !warnings.is_empty() {
                    println!("-- Analysis Warnings --");
                    for warn in warnings {
                        println!("{:?}", warn);
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
