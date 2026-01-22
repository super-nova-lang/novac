use std::fs::{self};
use std::path::PathBuf;

use clap::{Parser, Subcommand};
use miette::{IntoDiagnostic, WrapErr};

mod lexer;
mod parser;

fn main() -> miette::Result<()> {
    let args = Args::parse();
    match args.command {
        Command::Tokenize { filepath } => {
            let file_contents = fs::read_to_string(&filepath)
                .into_diagnostic()
                .wrap_err_with(|| format!("reading '{}' failed", filepath.display()))?;

            for token in lexer::Lexer::new(&file_contents) {
                let token = token?;
                println!("Found: {:?}", token);
            }
        }
        Command::Parse { filepath } => {
            let file_contents = fs::read_to_string(&filepath)
                .into_diagnostic()
                .wrap_err_with(|| format!("reading '{}' failed", filepath.display()))?;

            let parser = parser::Parser::new(&file_contents);
            let program = parser.parse()?;
            println!("{}", program);
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
