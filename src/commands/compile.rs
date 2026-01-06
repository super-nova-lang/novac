use anyhow::{Result, bail};
use std::path::Path;
use std::process::Command;

use crate::cli::Target;
use crate::commands::common::{
    analyze_step, lex_step, parse_step, read_source_with_stdlib, report_analysis_errors,
    report_parse_errors,
};

pub fn run(files: Vec<String>, target: Target) -> Result<()> {
    for file in files {
        let source = read_source_with_stdlib(&file)?;
        let tokens = lex_step(&file, &source);
        let ast = parse_step(tokens)?;

        if report_parse_errors(&ast) {
            continue;
        }

        let (errors, _warnings, _return_types) = analyze_step(ast.clone());

        if report_analysis_errors(&file, &errors) {
            continue;
        }

        match target {
            Target::Amd64 => match codegen::target_amd64::gen_target(&file, &ast) {
                Ok(asm) => {
                    let out_base = Path::new("build");
                    let emit_dir = out_base.join("emit");
                    let debug_dir = out_base.join("debug");
                    std::fs::create_dir_all(&emit_dir)?;
                    std::fs::create_dir_all(&debug_dir)?;
                    let stem = Path::new(&file)
                        .file_stem()
                        .and_then(|s| s.to_str())
                        .unwrap_or("output");
                    let asm_path = emit_dir.join(format!("{}.s", stem));
                    std::fs::write(asm_path.to_str().unwrap(), asm)?;

                    let out_path = debug_dir.join(format!("{}.out", stem));
                    match Command::new("clang")
                        .arg(asm_path.to_str().unwrap())
                        .arg("-nostartfiles")
                        .arg("-o")
                        .arg(out_path.to_str().unwrap())
                        .status()
                    {
                        Ok(s) if s.success() => {
                            println!("Linked executable: {}", out_path.display());
                        }
                        Ok(s) => {
                            println!("Linker failed for {}: status {}", file, s);
                        }
                        Err(e) => {
                            println!("Failed to invoke linker for {}: {}", file, e);
                        }
                    }
                }
                Err(e) => {
                    println!("Codegen failed for {}: {}", file, e);
                }
            },
        }
    }

    Ok(())
}
