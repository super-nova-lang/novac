use std::fs;
use std::path::PathBuf;

use miette::{IntoDiagnostic, Result, WrapErr};
use tracing::info;

use novac::lexer;

pub fn execute(filepath: PathBuf) -> Result<()> {
    let file_contents = fs::read_to_string(&filepath)
        .into_diagnostic()
        .wrap_err_with(|| format!("reading '{}' failed", filepath.display()))?;

    for token in lexer::Lexer::new(&file_contents) {
        let token = token?;
        info!("{:?}", token);
    }

    Ok(())
}
