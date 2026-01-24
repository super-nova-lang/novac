use std::fs;
use std::path::PathBuf;

use miette::{IntoDiagnostic, Result, WrapErr};
use tracing::info;

use novac::analyzer;
use novac::codegen;
use novac::parser;

pub fn execute(filepath: PathBuf) -> Result<()> {
    // Read source file
    let file_contents = fs::read_to_string(&filepath)
        .into_diagnostic()
        .wrap_err_with(|| format!("reading '{}' failed", &filepath.display()))?;

    // Parse
    let parser = parser::Parser::new(&file_contents);
    let program = parser.parse()?;

    // Analyze (type checking and inference)
    let annotated_program =
        analyzer::analyze(program, &file_contents).wrap_err("Type analysis failed")?;

    // Generate code from annotated program
    let generated = codegen::generate("main".to_string(), annotated_program, &file_contents)?;

    // Get base filename without extension
    let base_name = filepath
        .file_stem()
        .and_then(|s| s.to_str())
        .ok_or_else(|| {
            let path_display = filepath.display();
            miette::miette!("Invalid filename: {}", path_display)
        })?;

    // Create build directories
    let emit_dir = PathBuf::from("build/emit");
    let debug_dir = PathBuf::from("build/debug");

    fs::create_dir_all(&emit_dir)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to create directory: {}", emit_dir.display()))?;
    fs::create_dir_all(&debug_dir)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to create directory: {}", debug_dir.display()))?;

    // Write LLVM IR file
    let ll_path = emit_dir.join(format!("{}.ll", base_name));
    fs::write(&ll_path, generated)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to write LLVM IR file: {}", ll_path.display()))?;
    info!("Wrote LLVM IR to: {}", ll_path.display());

    // Compile LLVM IR to executable with clang
    let exe_path = debug_dir.join(base_name);
    let clang_output = std::process::Command::new("clang")
        .arg(&ll_path)
        .arg("-o")
        .arg(&exe_path)
        .arg("-Wno-override-module") // Suppress warnings about overriding modules
        .output()
        .into_diagnostic()
        .wrap_err("Failed to run clang. Is Clang installed?")?;

    if !clang_output.status.success() {
        let stderr = String::from_utf8_lossy(&clang_output.stderr);
        let stdout = String::from_utf8_lossy(&clang_output.stdout);
        return Err(miette::miette!(
            "Clang compilation failed:\n{}\n{}",
            stdout,
            stderr
        ));
    }
    info!("Built executable: {}", exe_path.display());

    Ok(())
}
