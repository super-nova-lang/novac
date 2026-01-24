pub mod build;
pub mod clean;
pub mod codegen;
pub mod doc;
pub mod parse;
pub mod run;
pub mod tokenize;

use std::path::PathBuf;
use miette::Result;

use clap::Subcommand;

#[derive(Subcommand)]
pub enum Command {
    Tokenize { filepath: PathBuf },
    Parse { filepath: PathBuf },
    Codegen { filepath: PathBuf },
    Build { filepath: PathBuf },
    Run { filepath: PathBuf },
    Clean,
    Doc { filepath: PathBuf },
}

pub fn execute_command(command: Command) -> Result<()> {
    match command {
        Command::Tokenize { filepath } => tokenize::execute(filepath),
        Command::Parse { filepath } => parse::execute(filepath),
        Command::Codegen { filepath } => codegen::execute(filepath),
        Command::Build { filepath } => build::execute(filepath),
        Command::Run { filepath } => run::execute(filepath),
        Command::Clean => clean::execute(),
        Command::Doc { filepath } => doc::execute(filepath),
    }
}
