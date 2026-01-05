use anyhow::Result;

use crate::commands::common::{lex_step, read_source};

pub fn run(files: Vec<String>) -> Result<()> {
    for file in files {
        let source = read_source(&file)?;
        let tokens = lex_step(&file, &source);
        for token in tokens {
            println!("{:?}", token);
        }
    }

    Ok(())
}
