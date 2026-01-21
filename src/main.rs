use std::fs::{self};
use std::path::PathBuf;

use lexer::Lexer;

use clap::{Parser, Subcommand};
use miette::{IntoDiagnostic, WrapErr};

mod lexer;

fn main() -> miette::Result<()> {
    let args = Args::parse();
    match args.command {
        Command::Tokenize { filepath } => {
            let file_contents = fs::read_to_string(&filepath)
                .into_diagnostic()
                .wrap_err_with(|| format!("reading '{}' failed", filepath.display()))?;

            for token in Lexer::new(&file_contents) {
                let token = token?;
                println!("Found: {:?}", token);
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
}
