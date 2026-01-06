use crate::logging;
use analysis::{self, AnalysisResult};
use anyhow::Result;
use lexer::{Lexer, token::Token};
use parser::{self, nodes::Node};

pub fn read_source(file: &str) -> Result<String> {
    std::fs::read_to_string(file).map_err(Into::into)
}

pub fn read_source_with_stdlib(file: &str) -> Result<String> {
    let mut source = String::new();

    // Include root.nova first (unmangled, globally accessible)
    if let Ok(root_content) = std::fs::read_to_string("stdlib/root.nova") {
        source.push_str(&root_content);
        source.push('\n');
    }

    // Wrap all stdlib files in a std module
    source.push_str("module std {\n");

    // Include each file in stdlib/std/ wrapped in its own module
    if let Ok(entries) = std::fs::read_dir("stdlib/std") {
        let mut files: Vec<_> = entries
            .filter_map(|e| e.ok())
            .filter(|e| {
                e.path()
                    .extension()
                    .map(|ext| ext == "nova")
                    .unwrap_or(false)
            })
            .collect();

        // Sort for consistent order
        files.sort_by_key(|e| e.path());

        for entry in files {
            let path = entry.path();
            if let Ok(content) = std::fs::read_to_string(&path) {
                // Get filename without extension to create module name
                let module_name = path
                    .file_stem()
                    .and_then(|s| s.to_str())
                    .unwrap_or("unknown");

                // Wrap file content in module <filename>
                source.push_str(&format!("    module {} {{\n", module_name));
                // Indent the content
                for line in content.lines() {
                    source.push_str("        ");
                    source.push_str(line);
                    source.push('\n');
                }
                source.push_str("    }\n\n");
            }
        }
    }

    source.push_str("}\n\n");

    // Append user source file
    let user_source = std::fs::read_to_string(file)?;
    source.push_str(&user_source);

    Ok(source)
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
            logging::error_span(&span.file, span.line, span.column, "parsing", msg);
            had_parse_error = true;
        }
    }
    had_parse_error
}

pub fn report_analysis_errors(file: &str, errors: &[AnalysisResult]) -> bool {
    if errors.is_empty() {
        return false;
    }

    logging::info("-- Analysis Errors --");
    for err in errors {
        if let AnalysisResult::Error(e) = err {
            match e {
                analysis::AnalysisError::UndefinedVariable(name, suggestions) => {
                    logging::error_simple(
                        file,
                        "analysis.undefined_variable",
                        &format!(
                            "Undefined variable '{}' (suggestions: {})",
                            name,
                            suggestions.join(", ")
                        ),
                    );
                }
                analysis::AnalysisError::TypeMismatch(a, b) => {
                    logging::error_simple(
                        file,
                        "analysis.type_mismatch",
                        &format!("Type mismatch: expected {:?}, found {:?}", a, b),
                    );
                }
                analysis::AnalysisError::DuplicateDeclaration(name, (r, c)) => {
                    logging::error_span(
                        file,
                        *r,
                        *c,
                        "analysis.duplicate_declaration",
                        &format!("Duplicate declaration '{}'", name),
                    );
                }
                analysis::AnalysisError::InvalidOperation(msg) => {
                    logging::error_simple(file, "analysis", msg);
                }
                analysis::AnalysisError::MissingReturnType(name) => {
                    logging::error_simple(
                        file,
                        "analysis.missing_return",
                        &format!("Missing return in function '{}'", name),
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

    logging::info("-- Analysis Warnings --");
    for warn in warnings {
        if let AnalysisResult::Warning(w) = warn {
            match w {
                analysis::AnalysisWarning::UnusedVariable(name) => {
                    logging::warn_simple(
                        file,
                        "analysis.unused_variable",
                        &format!("Unused variable '{}'", name),
                    );
                }
                analysis::AnalysisWarning::ShadowedVariable(name) => {
                    logging::warn_simple(
                        file,
                        "analysis.shadowed_variable",
                        &format!("Shadowed variable '{}'", name),
                    );
                }
            }
        }
    }
}
