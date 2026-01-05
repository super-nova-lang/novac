#![allow(dead_code, unused_imports)]

use anyhow::Result;
use clap::Parser as _;

use cli::Cli;

mod cli;
mod commands;

fn main() -> Result<()> {
    let cli = Cli::parse();

    match cli {
        Cli::Tokenize { files } => commands::tokenize::run(files)?,
        Cli::Parse { files } => commands::parse::run(files)?,
        Cli::Codegen { files } => commands::codegen::run(files)?,
        Cli::Compile { files } => commands::compile::run(files)?,
        Cli::Run { files } => commands::run::run(files)?,
        Cli::Clean => commands::clean::run()?,
        Cli::Doc { files } => commands::doc::run(files)?,
        Cli::TestCompile => commands::test_compile::run()?,
        Cli::TestCompilePromote => commands::test_compile_promote::run()?,
    }

    Ok(())
}
