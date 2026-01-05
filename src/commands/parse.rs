use anyhow::Result;

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

        println!("-- AST --\n{:#?}", ast);
        report_analysis_errors(&file, &errors);
        report_analysis_warnings(&file, &warnings);
    }

    Ok(())
}
