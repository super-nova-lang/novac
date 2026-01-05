use anyhow::{Result, bail};
use std::path::Path;
use std::process::Command;

use crate::cli::Target;
use crate::commands::common::{
    analyze_step, lex_step, parse_step, read_source, report_parse_errors,
};

pub fn run(files: Vec<String>, target: Target) -> Result<()> {
    for file in files {
        let source = read_source(&file)?;
        let tokens = lex_step(&file, &source);
        let ast = parse_step(tokens)?;

        if report_parse_errors(&ast) {
            continue;
        }

        let (errors, _warnings) = analyze_step(ast.clone());

        if !errors.is_empty() {
            println!("-- Analysis Errors --");
            continue;
        }

        match target {
            Target::Llvm => match codegen::target_llvm::gen_target(&file, &ast) {
                Ok(ir) => {
                    let out_base = Path::new("build");
                    let emit_dir = out_base.join("emit");
                    let debug_dir = out_base.join("debug");
                    std::fs::create_dir_all(&emit_dir)?;
                    std::fs::create_dir_all(&debug_dir)?;
                    let stem = Path::new(&file)
                        .file_stem()
                        .and_then(|s| s.to_str())
                        .unwrap_or("output");
                    let ll_path = emit_dir.join(format!("{}.ll", stem));
                    std::fs::write(ll_path.to_str().unwrap(), ir)?;

                    let out_path = debug_dir.join(format!("{}.out", stem));
                    match Command::new("clang")
                        .arg(ll_path.to_str().unwrap())
                        .arg("-o")
                        .arg(out_path.to_str().unwrap())
                        .status()
                    {
                        Ok(s) if s.success() => {
                            println!("Linked executable: {}", out_path.display());
                            match Command::new(out_path.to_str().unwrap()).status() {
                                Ok(rs) => {
                                    println!("Execution finished: {}", rs);
                                }
                                Err(e) => {
                                    println!("Failed to execute {}: {}", out_path.display(), e);
                                }
                            }
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
            Target::Amd64 => match codegen::target_amd64::gen_target(&file, &ast) {
                Ok(ir) => {
                    let out_base = Path::new("build");
                    let emit_dir = out_base.join("emit");
                    let debug_dir = out_base.join("debug");
                    std::fs::create_dir_all(&emit_dir)?;
                    std::fs::create_dir_all(&debug_dir)?;
                    let stem = Path::new(&file)
                        .file_stem()
                        .and_then(|s| s.to_str())
                        .unwrap_or("output");
                    let ll_path = emit_dir.join(format!("{}.ll", stem));
                    std::fs::write(ll_path.to_str().unwrap(), ir)?;

                    let out_path = debug_dir.join(format!("{}.out", stem));
                    match Command::new("clang")
                        .arg(ll_path.to_str().unwrap())
                        .arg("-o")
                        .arg(out_path.to_str().unwrap())
                        .status()
                    {
                        Ok(s) if s.success() => {
                            println!("Linked executable: {}", out_path.display());
                            match Command::new(out_path.to_str().unwrap()).status() {
                                Ok(rs) => {
                                    println!("Execution finished: {}", rs);
                                }
                                Err(e) => {
                                    println!("Failed to execute {}: {}", out_path.display(), e);
                                }
                            }
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
