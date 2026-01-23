use std::fs;
use std::path::PathBuf;

use miette::{IntoDiagnostic, Result, WrapErr};
use tracing::info;

use novac::analyzer;
use novac::parser;

pub fn execute(filepath: PathBuf) -> Result<()> {
    let file_contents = fs::read_to_string(&filepath)
        .into_diagnostic()
        .wrap_err_with(|| format!("reading '{}' failed", filepath.display()))?;

    let parser = parser::Parser::new(&file_contents);
    let program = parser.parse()?;

    info!("=== Parsed AST ===");
    let display = format!("{:#?}", program);
    for line in display.lines() {
        info!("{}", line)
    }

    // Run type analysis
    info!("=== Type Analysis ===");
    match analyzer::analyze(program, &file_contents) {
        Ok(annotated) => {
            info!("Type analysis successful!");
            let annotated_display = format!("{:#?}", annotated);
            for line in annotated_display.lines() {
                info!("{}", line)
            }
        }
        Err(e) => {
            return Err(e).wrap_err("Type analysis failed");
        }
    }

    Ok(())
}
