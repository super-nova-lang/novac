use std::path::PathBuf;
use std::process::Command;

use miette::{IntoDiagnostic, Result, WrapErr};
use tracing::info;

use crate::commands::build;

pub fn execute(filepath: PathBuf) -> Result<()> {
    // First, build the file
    build::execute(filepath.clone())?;

    // Get base filename without extension
    let base_name = filepath
        .file_stem()
        .and_then(|s| s.to_str())
        .ok_or_else(|| {
            let path_display = filepath.display();
            miette::miette!("Invalid filename: {}", path_display)
        })?;

    // Get executable path
    let exe_path = PathBuf::from("build/debug").join(base_name);

    info!("Running executable: {}", exe_path.display());

    // Run the executable
    let status = Command::new(&exe_path)
        .status()
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to run executable: {}", exe_path.display()))?;

    // Exit with the same code as the program
    std::process::exit(status.code().unwrap_or(1));
}
