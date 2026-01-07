use clap::{Parser, ValueEnum};

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
    Codegen {
        #[arg(short, long, value_enum, default_value_t = Target::Amd64)]
        target: Target,
        files: Vec<String>,
    },
    /// Compile the Nova source code
    Compile {
        #[arg(short, long, value_enum, default_value_t = Target::Amd64)]
        target: Target,
        files: Vec<String>,
    },
    // UTILITY COMMANDS
    /// Clean build artifacts
    Clean,
    /// Generate documentation for Nova source code
    Doc { files: Vec<String> },
    /// Run Nova project
    Run {
        #[arg(short, long, value_enum, default_value_t = Target::Amd64)]
        target: Target,
        files: Vec<String>,
    },
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
        }
    }
}

#[derive(Debug, Clone, Copy, ValueEnum)]
pub enum Target {
    #[value(name = "amd64")]
    Amd64,
}
