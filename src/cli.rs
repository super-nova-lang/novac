use clap::Parser;

#[derive(Debug, Parser)]
#[command(
    name = "novac",
    version = "0.1.0",
    author = "Ashton Sickler <ashton.sickler06@gmail.com>",
    about = "Nova Compiler"
)]
pub enum Cli {
    // PIPELINE STEPS
    /// Tokenize Nova source code and output the AST
    Tokenize { files: Vec<String> },
    /// Parse Nova source code and output the AST
    Parse { files: Vec<String> },
    /// Generate the LLVM source code
    Codegen { files: Vec<String> },
    /// Compile the Nova source code
    Compile { files: Vec<String> },
    // UTILITY COMMANDS
    /// Clean build artifacts
    Clean,
    /// Generate documentation for Nova source code
    Doc { files: Vec<String> },
    /// Run Nova project
    Run { files: Vec<String> },
    // DEVELOPMENT COMMANDS
    /// Test the compiler
    TestCompile,
    /// Test the compiler with promotion enabled
    TestCompilePromote,
}

impl std::fmt::Display for Cli {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Cli::Tokenize { .. } => write!(f, "tokenize"),
            Cli::Clean => write!(f, "clean"),
            Cli::Parse { .. } => write!(f, "parse"),
            Cli::Codegen { .. } => write!(f, "codegen"),
            Cli::Compile { .. } => write!(f, "compile"),
            Cli::Doc { .. } => write!(f, "doc"),
            Cli::Run { .. } => write!(f, "run"),
            Cli::TestCompile => write!(f, "test-compile"),
            Cli::TestCompilePromote => write!(f, "test-compile-promote"),
        }
    }
}
