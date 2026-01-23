use std::fs::{self};
use std::path::PathBuf;

use clap::{Parser, Subcommand};
use miette::{IntoDiagnostic, WrapErr};
use tracing::info;
use tracing_subscriber::EnvFilter;
use tracing_subscriber::Layer;
use tracing_subscriber::Registry;
use tracing_subscriber::layer::SubscriberExt;
use tracing_tree::HierarchicalLayer;

mod analyzer;
mod codegen;
mod lexer;
mod parser;

fn main() -> miette::Result<()> {
    // Parse arguments
    let args = Args::parse();

    // Set up subscriber
    let debug = if args.debug {
        true
    } else {
        std::env::var("NOVAC_DEBUG")
            .map(|v| {
                let v = v.to_lowercase();
                v == "1" || v == "true" || v == "yes"
            })
            .unwrap_or(false)
    };

    let hierarchical = HierarchicalLayer::default()
        .with_deferred_spans(true)
        .with_filter(get_env_filter(debug));
    let subscriber = Registry::default().with(hierarchical);
    tracing::subscriber::set_global_default(subscriber).unwrap();

    // Act on arguments
    match args.command {
        Command::Tokenize { filepath } => {
            let file_contents = fs::read_to_string(&filepath)
                .into_diagnostic()
                .wrap_err_with(|| format!("reading '{}' failed", filepath.display()))?;

            for token in lexer::Lexer::new(&file_contents) {
                let token = token?;
                info!("{:?}", token);
            }
        }
        Command::Parse { filepath } => {
            let file_contents = fs::read_to_string(&filepath)
                .into_diagnostic()
                .wrap_err_with(|| format!("reading '{}' failed", filepath.display()))?;

            let parser = parser::Parser::new(&file_contents);
            let program = parser.parse()?;
            
            info!("=== Parsed AST ===");
            let display = format!("{:#?}", program);
            for line in display.lines() {
                info!("{}", line)
            }
            
            // Run type analysis
            info!("=== Type Analysis ===");
            match analyzer::analyze(program, &file_contents) {
                Ok(annotated) => {
                    info!("Type analysis successful!");
                    let annotated_display = format!("{:#?}", annotated);
                    for line in annotated_display.lines() {
                        info!("{}", line)
                    }
                }
                Err(e) => {
                    return Err(e)
                        .wrap_err("Type analysis failed");
                }
            }
        }
        Command::Codegen { filepath } => {
            let file_contents = fs::read_to_string(&filepath)
                .into_diagnostic()
                .wrap_err_with(|| format!("reading '{}' failed", filepath.display()))?;

            // Parse
            let parser = parser::Parser::new(&file_contents);
            let program = parser.parse()?;
            
            // Analyze (type checking and inference)
            let annotated_program = analyzer::analyze(program, &file_contents)
                .wrap_err("Type analysis failed")?;
            
            // Generate code from annotated program
            let cg = codegen::Codegen::from_annotated_program("main".to_string(), annotated_program)?;
            let generated = cg.emit();
            info!("Generated: {}", generated);
        }
    }
    Ok(())
}

#[derive(Parser)]
struct Args {
    /// Enable debug/trace logging
    #[arg(short, long)]
    debug: bool,

    #[command(subcommand)]
    command: Command,
}

#[derive(Subcommand)]
enum Command {
    Tokenize { filepath: PathBuf },
    Parse { filepath: PathBuf },
    Codegen { filepath: PathBuf },
}

fn get_env_filter(debug: bool) -> EnvFilter {
    if debug {
        EnvFilter::new("trace")
    } else {
        EnvFilter::new("info")
    }
}
