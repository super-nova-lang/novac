use std::fs;
use std::path::PathBuf;

use miette::{IntoDiagnostic, Result, WrapErr};
use tracing::info;

use novac::analyzer;
use novac::codegen;
use novac::parser;

pub fn execute(filepath: PathBuf) -> Result<()> {
    let file_contents = fs::read_to_string(&filepath)
        .into_diagnostic()
        .wrap_err_with(|| format!("reading '{}' failed", filepath.display()))?;

    // Parse
    let parser = parser::Parser::new(&file_contents);
    let program = parser.parse()?;

    // Analyze (type checking and inference)
    let annotated_program =
        analyzer::analyze(program, &file_contents).wrap_err("Type analysis failed")?;

    // Generate code from annotated program
    let generated = codegen::generate("main".to_string(), annotated_program, &file_contents)?;
    info!("Generated: {}", generated);

    Ok(())
}
