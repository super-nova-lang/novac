# `novac` - the Supernova compiler

`novac` is a Rust-based compiler for the Supernova language with a CLI that lexes, parses, analyzes, and generates LLVM IR.

## Quick Start

```bash
cargo build
cargo run -- run examples/showcase.nova
cargo test
```

## Testing

Tests run the full compiler pipeline (tokenize → parse → analyze → codegen → run) on `.nova` files in the `tests/` directory and compare outputs against `.expected.json` baseline files. Expected files are automatically generated on first run. Each test captures exit codes and output from each pipeline step.

```bash
cargo test --test integration_test
```

## Pipeline

- **Lexer** (crate `lexer/`) — Tokenization ✓
- **Parser** (crate `parser/`) — AST generation ✓
- **Analysis** (crate `analysis/`) — Semantic analysis ✓
- **Codegen** (crate `codegen/`) — LLVM IR generation (WIP)

## Structure

- `src/` — CLI entry and subcommands
- `examples/` — Sample `.nova` programs
- `tests/` — Integration tests
- `stdlib/` — Standard library

## Prerequisites

- Rust 2024 edition
