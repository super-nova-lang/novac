# Novac - The Supernova Language Compiler

`novac` is a compiler for the Supernova programming language, implemented in Rust. It provides a complete compilation pipeline from source code to LLVM IR, with support for lexical analysis, parsing, semantic analysis, and code generation.

## Features

- **Lexical Analysis**: Tokenizes Supernova source code into a stream of tokens
- **Parsing**: Constructs an Abstract Syntax Tree (AST) from tokens
- **Semantic Analysis**: Performs type checking and semantic validation with error/warning reporting
- **Code Generation**: Generates LLVM IR using `inkwell` (LLVM 21.1)
- **CLI Interface**: Easy-to-use command-line interface for all compiler stages

## Language Features

Supernova supports:
- Struct and enum types with associated methods
- Pattern matching and destructuring
- Type annotations and inference
- Module system with `open` statements
- C interop for external libraries

See [`examples/showcase.nova`](examples/showcase.nova) for a feature demonstration.

## Installation

### Prerequisites

- Rust toolchain (2024 edition)
- LLVM 21.1 development libraries (required for `inkwell`)

### Building from Source

```bash
cargo build --release
```

The compiled binary will be available at `target/release/novac`.

## Usage

### Tokenize

Tokenize a Supernova source file and output tokens:

```bash
cargo run -- tokenize examples/showcase.nova
```

### Parse

Parse a Supernova source file and output the AST with analysis results:

```bash
cargo run -- parse examples/showcase.nova
```

### Code Generation

Generate LLVM IR from a Supernova source file:

```bash
cargo run -- codegen examples/showcase.nova
```

### Other Commands

```bash
cargo run -- compile <files>...           # Compile source files
cargo run -- run                          # Run a Nova project
cargo run -- test-compile                 # Test the compiler
cargo run -- doc                          # Generate documentation
cargo run -- clean                        # Clean build artifacts
```

## Project Structure

This project is organized as a Cargo workspace with multiple crates:

```
novac-rust/
├── src/                # Binary crate - CLI entry point and pipeline orchestration
├── lexer/              # Lexer crate - tokenization
├── parser/             # Parser crate - AST construction
├── analysis/           # Semantic analysis - type checking and validation
├── codegen/            # LLVM IR code generation using inkwell
├── stdlib/             # Standard library (work in progress)
├── examples/           # Example .nova programs
├── tests/              # Integration tests
└── Cargo.toml          # Workspace configuration
```

### Crate Dependencies

```
novac (binary)
├── lexer
├── parser → lexer
├── analysis → parser
└── codegen → parser, analysis, inkwell
```

## Development

### Running Tests

```bash
cargo test
```

### Testing Specific Compiler Stages

```bash
# Tokenization only
cargo run -- tokenize examples/showcase.nova

# Parsing and analysis
cargo run -- parse examples/showcase.nova

# Full pipeline to LLVM IR
cargo run -- codegen examples/showcase.nova
```

## Example Program

```nova
(* Novalang Showcase *)
open std.c with { stdio.printf }

let Job :: title, salary = enum {
    programmer :: struct { lang: string },
    other      :: string,
    sales_man  :: (),
}

let Person :: name, age, job = struct {
    name: string = name,
    age : i32 = age,
    job : Job = job
} with {
    let greet :: self = printf("Hello! My name is %s and I am %d years old\n", self.name, self.age)
}

let main :: () = {  
    let engineer = Job.programmer("Programmer", 120000, "C++")
    let p1 = Person("Ashton", 19, engineer)
    p1.greet()
    printf("Salary: %d\n", p1.job.salary)
    ()
}
```

## Compiler Pipeline

1. **Lexical Analysis** (`lexer` crate)
   - Input: Source code string
   - Output: Token stream

2. **Parsing** (`parser` crate)
   - Input: Token stream
   - Output: Abstract Syntax Tree (AST)

3. **Semantic Analysis** (`analysis` crate)
   - Input: AST
   - Output: Validated AST + errors/warnings

4. **Code Generation** (`codegen` crate)
   - Input: Validated AST
   - Output: LLVM IR Module

## Contributing

Conventions:
- Keep `.nova` examples in `examples/` and mirror them in `tests/` when needed
- Prefer small, focused crates for each compiler stage
- Use descriptive error messages with source location information
- Run `cargo test` before submitting changes

## License

[Add license information here]

## Roadmap

- [ ] Complete standard library implementation
- [ ] Optimization passes
- [ ] Native binary generation
- [ ] Package manager integration
- [ ] IDE language server protocol support
- [ ] Comprehensive error reporting with source code snippets

## Version

Current version: 0.1.0
