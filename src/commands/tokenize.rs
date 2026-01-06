use anyhow::Result;

use crate::commands::common::{lex_step, read_source_with_stdlib};

pub fn run(files: Vec<String>) -> Result<()> {
    for file in files {
        let source = read_source_with_stdlib(&file)?;
        let tokens = lex_step(&file, &source);
        for token in tokens {
            println!("{:?}", token);
        }
    }

    Ok(())
}
