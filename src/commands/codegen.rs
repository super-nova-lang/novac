use anyhow::{Result, bail};

use crate::cli::Target;
use crate::commands::common::{
    analyze_step, lex_step, parse_step, read_source, report_analysis_errors,
    report_analysis_warnings, report_parse_errors,
};

pub fn run(files: Vec<String>, target: Target) -> Result<()> {
    for file in files {
        let source = read_source(&file)?;
        let tokens = lex_step(&file, &source);
        let ast = parse_step(tokens)?;

        if report_parse_errors(&ast) {
            continue;
        }

        let (errors, warnings) = analyze_step(ast.clone());

        if report_analysis_errors(&file, &errors) {
            continue;
        }
        report_analysis_warnings(&file, &warnings);

        match target {
            Target::Llvm => match codegen::target_llvm::gen_target(&file, &ast) {
                Ok(ir) => println!("{}", ir),
                Err(e) => println!("Codegen failed for {}: {}", file, e),
            },
            Target::Amd64 => {
                bail!("target amd64 not implemented yet");
            }
        }
    }

    Ok(())
}
