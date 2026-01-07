#![allow(dead_code, unused_imports)]

use anyhow::Result;
use clap::Parser as _;

use cli::Cli;

mod cli;
mod commands;
mod logging;

fn main() -> Result<()> {
    logging::init();
    let cli = Cli::parse();

    match cli {
        Cli::Tokenize { files } => commands::tokenize::run(files)?,
        Cli::Parse { files } => commands::parse::run(files)?,
        Cli::Codegen { files, target } => commands::codegen::run(files, target)?,
        Cli::Compile { files, target } => commands::compile::run(files, target)?,
        Cli::Run { files, target } => commands::run::run(files, target)?,
        Cli::Clean => commands::clean::run()?,
        Cli::Doc { files } => commands::doc::run(files)?,
    }

    Ok(())
}
