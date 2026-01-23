use std::fs;
use std::path::PathBuf;

use miette::{IntoDiagnostic, Result, WrapErr};
use tracing::info;

pub fn execute() -> Result<()> {
    let emit_dir = PathBuf::from("build/emit");
    let debug_dir = PathBuf::from("build/debug");

    // Remove emit directory if it exists
    if emit_dir.exists() {
        fs::remove_dir_all(&emit_dir)
            .into_diagnostic()
            .wrap_err_with(|| format!("Failed to remove directory: {}", emit_dir.display()))?;
        info!("Removed directory: {}", emit_dir.display());
    }

    // Remove debug directory if it exists
    if debug_dir.exists() {
        fs::remove_dir_all(&debug_dir)
            .into_diagnostic()
            .wrap_err_with(|| format!("Failed to remove directory: {}", debug_dir.display()))?;
        info!("Removed directory: {}", debug_dir.display());
    }

    Ok(())
}
