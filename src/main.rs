use std::fs::{self};
use std::path::{Path, PathBuf};

use clap::{Parser, Subcommand};
use miette::{IntoDiagnostic, WrapErr};
use tracing::info;
use tracing_subscriber::EnvFilter;
use tracing_subscriber::Layer;
use tracing_subscriber::Registry;
use tracing_subscriber::layer::SubscriberExt;
use tracing_tree::HierarchicalLayer;

mod analyzer;
mod codegen;
mod lexer;
mod parser;

fn main() -> miette::Result<()> {
    // Parse arguments
    let args = Args::parse();

    // Set up subscriber
    let debug = if args.debug {
        true
    } else {
        std::env::var("NOVAC_DEBUG")
            .map(|v| {
                let v = v.to_lowercase();
                v == "1" || v == "true" || v == "yes"
            })
            .unwrap_or(false)
    };

    let hierarchical = HierarchicalLayer::default()
        .with_deferred_spans(true)
        .with_filter(get_env_filter(debug));
    let subscriber = Registry::default().with(hierarchical);
    tracing::subscriber::set_global_default(subscriber).unwrap();

    // Act on arguments
    match args.command {
        Command::Tokenize { filepath } => {
            let file_contents = fs::read_to_string(&filepath)
                .into_diagnostic()
                .wrap_err_with(|| format!("reading '{}' failed", filepath.display()))?;

            for token in lexer::Lexer::new(&file_contents) {
                let token = token?;
                info!("{:?}", token);
            }
        }
        Command::Parse { filepath } => {
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
                    return Err(e)
                        .wrap_err("Type analysis failed");
                }
            }
        }
        Command::Codegen { filepath } => {
            let file_contents = fs::read_to_string(&filepath)
                .into_diagnostic()
                .wrap_err_with(|| format!("reading '{}' failed", filepath.display()))?;

            // Parse
            let parser = parser::Parser::new(&file_contents);
            let program = parser.parse()?;
            
            // Analyze (type checking and inference)
            let annotated_program = analyzer::analyze(program, &file_contents)
                .wrap_err("Type analysis failed")?;
            
            // Generate code from annotated program
            let cg = codegen::Codegen::from_annotated_program("main".to_string(), annotated_program)?;
            let generated = cg.emit();
            info!("Generated: {}", generated);
        }
        Command::Build { filepath } => {
            build_command(&filepath)?;
        }
        Command::Clean => {
            clean_command()?;
        }
    }
    Ok(())
}

#[derive(Parser)]
struct Args {
    /// Enable debug/trace logging
    #[arg(short, long)]
    debug: bool,

    #[command(subcommand)]
    command: Command,
}

#[derive(Subcommand)]
enum Command {
    Tokenize { filepath: PathBuf },
    Parse { filepath: PathBuf },
    Codegen { filepath: PathBuf },
    Build { filepath: PathBuf },
    Clean,
}

fn get_env_filter(debug: bool) -> EnvFilter {
    if debug {
        EnvFilter::new("trace")
    } else {
        EnvFilter::new("info")
    }
}

fn build_command(filepath: &Path) -> miette::Result<()> {
    // Read source file
    let file_contents = fs::read_to_string(filepath)
        .into_diagnostic()
        .wrap_err_with(|| format!("reading '{}' failed", filepath.display()))?;

    // Parse
    let parser = parser::Parser::new(&file_contents);
    let program = parser.parse()?;
    
    // Analyze (type checking and inference)
    let annotated_program = analyzer::analyze(program, &file_contents)
        .wrap_err("Type analysis failed")?;
    
    // Generate code from annotated program
    let cg = codegen::Codegen::from_annotated_program("main".to_string(), annotated_program)?;
    let assembly = cg.emit();

    // Check if _start exists in the assembly (i.e., if there's a main function)
    let has_start = assembly.contains("global _start") || assembly.contains("_start:");

    // Get base filename without extension
    let base_name = filepath
        .file_stem()
        .and_then(|s| s.to_str())
        .ok_or_else(|| miette::miette!("Invalid filename: {}", filepath.display()))?;

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
        .arg("-nostartfiles")  // Don't use C runtime's _start, use our own
        .arg("-no-pie")         // Position-independent executable can cause issues
        .arg("-e")
        .arg("_start")          // Use our _start as entry point
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

fn clean_command() -> miette::Result<()> {
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
