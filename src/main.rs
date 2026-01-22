use std::fs::{self};
use std::path::PathBuf;

use clap::{Parser, Subcommand};
use miette::{IntoDiagnostic, WrapErr};
use tracing::info;
use tracing_subscriber::EnvFilter;
use tracing_subscriber::Registry;
use tracing_subscriber::layer::SubscriberExt;
use tracing_tree::HierarchicalLayer;

mod lexer;
mod parser;

fn main() -> miette::Result<()> {
    // Parse arguments
    let args = Args::parse();

    // Set up subscriber
    let subscriber = Registry::default()
        .with(get_env_filter())
        .with(HierarchicalLayer::default());
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
            let display = format!("{:#?}", program);
            for line in display.lines() {
                info!("{}", line)
            }
        }
    }
    Ok(())
}

#[derive(Parser)]
struct Args {
    #[command(subcommand)]
    command: Command,
}

#[derive(Subcommand)]
enum Command {
    Tokenize { filepath: PathBuf },
    Parse { filepath: PathBuf },
}

fn get_env_filter() -> EnvFilter {
    let debug = std::env::var("NOVAC_DEBUG")
        .map(|v| {
            let v = v.to_lowercase();
            v == "1" || v == "true" || v == "yes"
        })
        .unwrap_or(false);
    if debug {
        EnvFilter::new("trace")
    } else {
        EnvFilter::new("info")
    }
}
