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
    let cg = codegen::Codegen::from_annotated_program("main".to_string(), annotated_program)?;
    let assembly = cg.emit();

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

    // Write assembly file
    let asm_path = emit_dir.join(format!("{}.asm", base_name));
    fs::write(&asm_path, assembly)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to write assembly file: {}", asm_path.display()))?;
    info!("Wrote assembly to: {}", asm_path.display());

    // Assemble with NASM
    let obj_path = emit_dir.join(format!("{}.o", base_name));
    let nasm_output = std::process::Command::new("nasm")
        .arg("-f")
        .arg("elf64")
        .arg(&asm_path)
        .arg("-o")
        .arg(&obj_path)
        .output()
        .into_diagnostic()
        .wrap_err("Failed to run nasm. Is NASM installed?")?;

    if !nasm_output.status.success() {
        let stderr = String::from_utf8_lossy(&nasm_output.stderr);
        return Err(miette::miette!("NASM assembly failed:\n{}", stderr));
    }
    info!("Assembled object file: {}", obj_path.display());

    // Link with gcc (to get libc for printf/abort)
    // Always use -nostartfiles and -e _start since we always generate _start
    let exe_path = debug_dir.join(base_name);
    let gcc_output = std::process::Command::new("gcc")
        .arg(&obj_path)
        .arg("-o")
        .arg(&exe_path)
        .arg("-nostartfiles") // Don't use C runtime's _start, use our own
        .arg("-no-pie") // Position-independent executable can cause issues
        .arg("-e")
        .arg("_start") // Use our _start as entry point
        .output()
        .into_diagnostic()
        .wrap_err("Failed to run gcc. Is GCC installed?")?;

    if !gcc_output.status.success() {
        let stderr = String::from_utf8_lossy(&gcc_output.stderr);
        return Err(miette::miette!("Linking failed:\n{}", stderr));
    }
    info!("Built executable: {}", exe_path.display());

    Ok(())
}
