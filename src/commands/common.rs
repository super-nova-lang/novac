use analysis::{self, AnalysisResult};
use anyhow::Result;
use lexer::{Lexer, token::Token};
use parser::{self, nodes::Node};

pub fn read_source(file: &str) -> Result<String> {
    std::fs::read_to_string(file).map_err(Into::into)
}

pub fn lex_step(file: &str, source: &str) -> Vec<Token> {
    Lexer::new(file, source).tokenize()
}

pub fn parse_step(tokens: Vec<Token>) -> Result<Vec<Node>> {
    parser::parse(tokens).map_err(|e| anyhow::anyhow!("parse error: {:?}", e))
}

pub fn analyze_step(nodes: Vec<Node>) -> (Vec<AnalysisResult>, Vec<AnalysisResult>) {
    analysis::analyze(nodes)
}

pub fn report_parse_errors(ast: &[Node]) -> bool {
    let mut had_parse_error = false;
    for node in ast {
        if let Node::Error(msg, span) = node {
            println!(
                "{}:{}:{} [error.parsing] {}",
                span.file, span.line, span.column, msg
            );
            had_parse_error = true;
        }
    }
    had_parse_error
}

pub fn report_analysis_errors(file: &str, errors: &[AnalysisResult]) -> bool {
    if errors.is_empty() {
        return false;
    }

    println!("-- Analysis Errors --");
    for err in errors {
        if let AnalysisResult::Error(e) = err {
            match e {
                analysis::AnalysisError::UndefinedVariable(name, suggestions) => {
                    println!(
                        "{}:0:0 [error.analysis.undefined_variable] Undefined variable '{}' (suggestions: {})",
                        file,
                        name,
                        suggestions.join(", ")
                    );
                }
                analysis::AnalysisError::TypeMismatch(a, b) => {
                    println!(
                        "{}:0:0 [error.analysis.type_mismatch] Type mismatch: expected {:?}, found {:?}",
                        file, a, b
                    );
                }
                analysis::AnalysisError::DuplicateDeclaration(name, (r, c)) => {
                    println!(
                        "{}:{}:{} [error.analysis.duplicate_declaration] Duplicate declaration '{}'",
                        file, r, c, name
                    );
                }
                analysis::AnalysisError::InvalidOperation(msg) => {
                    println!("{}:0:0 [error.analysis] {}", file, msg);
                }
                analysis::AnalysisError::MissingReturnType(name) => {
                    println!(
                        "{}:0:0 [error.analysis.missing_return] Missing return in function '{}'",
                        file, name
                    );
                }
            }
        }
    }

    true
}

pub fn report_analysis_warnings(file: &str, warnings: &[AnalysisResult]) {
    if warnings.is_empty() {
        return;
    }

    println!("-- Analysis Warnings --");
    for warn in warnings {
        if let AnalysisResult::Warning(w) = warn {
            match w {
                analysis::AnalysisWarning::UnusedVariable(name) => {
                    println!(
                        "{}:0:0 [warning.analysis.unused_variable] Unused variable '{}'",
                        file, name
                    );
                }
                analysis::AnalysisWarning::ShadowedVariable(name) => {
                    println!(
                        "{}:0:0 [warning.analysis.shadowed_variable] Shadowed variable '{}'",
                        file, name
                    );
                }
            }
        }
    }
}
