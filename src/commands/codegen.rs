use anyhow::{Result, bail};

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
            Target::Amd64 => match codegen::target_amd64_linux::gen_target(&file, &ast) {
                Ok(ir) => println!("{}", ir),
                Err(e) => println!("Codegen failed for {}: {}", file, e),
            },
        }
    }

    Ok(())
}
