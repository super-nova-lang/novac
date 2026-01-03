# Supernova Compiler (`novac`)

`novac` is the compiler for the Supernova language, implemented in Rust. It exposes a CLI that can lex, parse, analyze, and (work-in-progress) generate LLVM IR for `.nova` files.

## Workspace layout

- `src/`: Binary crate entry (`main.rs`, CLI wiring).
- `lexer/`: Lexer crate producing tokens.
- `parser/`: Parser crate producing AST nodes.
- `analysis/`: Semantic analysis over AST nodes.
- `codegen/`: LLVM IR generation using `inkwell` (early stage).
- `examples/`: Sample `.nova` programs.
- `tests/`: Integration samples used by the CLI.

## Build

```bash
cargo build
```

## Run (CLI)

```bash
cargo run -- parse examples/showcase.nova
```

Current subcommands are wired for lexing/parsing/analysis; codegen is under active development.

## Tests

```bash
cargo test
```

## Prerequisites

- Rust toolchain (2024 edition).
- LLVM development libraries for the `inkwell` dependency (version matching `inkwell` 0.7.x, e.g., LLVM 18).

## Conventions

- Keep `.nova` examples in `examples/` and mirror them in `tests/` when needed.
- Prefer small, focused crates for each compiler stage.
