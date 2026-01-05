use anyhow::Result;
use inkwell::context::Context;

use crate::commands::common::{
    analyze_step, lex_step, parse_step, read_source, report_analysis_errors, report_analysis_warnings,
    report_parse_errors,
};

pub fn run(files: Vec<String>) -> Result<()> {
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

        let context = Context::create();
        let codegen = codegen::Codegen::new(&context, &file);
        match codegen.compile(&ast) {
            Ok(module) => {
                println!("{}", module.print_to_string().to_string());
            }
            Err(e) => {
                println!("Codegen failed for {}: {}", file, e);
            }
        }
    }

    Ok(())
}
