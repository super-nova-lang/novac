# Supernova Compiler (`novac`)

`novac` is the compiler for the Supernova programming language, written in OCaml. It provides a lexer, parser, and code generator for the Supernova language.

## Project Structure

*   **`bin/`**: Contains the main entry point for the compiler executable.
    *   `main.ml`: The CLI entry point that supports lexing, parsing, code generation, and compilation.
    *   `dune`: Build configuration for the executable.
*   **`lib/`**: Contains the core compiler logic.
    *   `lexer.ml`: The lexer implementation.
    *   `parser.ml`: The parser implementation.
    *   `ast.ml`: AST node definitions.
    *   `token.ml`: Token definitions.
    *   `codegen.ml`: LLVM code generation logic.
    *   `logger.ml`: Logging configuration using `easy_logging`.
    *   `utils.ml`: Common utility functions.
    *   `test.ml`: Expectation tests using `ppx_expect`.
    *   `dune`: Build configuration for the library.
*   **`examples/`**: Contains example `.nova` source files used for testing and demonstration.

## Building and Running

This project uses `dune` for building and testing.

### Prerequisites

*   OCaml
*   Opam (OCaml Package Manager)
*   Dune
*   LLVM (for code generation)
*   Clang (for compilation)

### Build

To build the project:

```bash
dune build
```

### Run

The compiler provides several commands:

*   **Lexing:** `dune exec bin/main.exe -- lex <path_to_nova_file>`
*   **Parsing:** `dune exec bin/main.exe -- parse <path_to_nova_file>`
*   **Code Generation:** `dune exec bin/main.exe -- codegen <path_to_nova_file>`
*   **Compilation:** `dune exec bin/main.exe -- compile <path_to_nova_file>`

Example:

```bash
dune exec bin/main.exe -- compile examples/basic-functions.nova
```

### Test

To run the tests:

```bash
dune runtest
```

Tests are defined in `lib/test.ml` and automatically use files in `examples/`. The testing framework is `ppx_expect`. If the output changes intentionally, you can promote the new output as the expected output using:

```bash
dune promote
```

## Development Conventions

*   **Language:** OCaml.
*   **Formatting:** The project uses `.ocamlformat` for code formatting.
*   **Testing:** New features should include corresponding example files in `examples/` and be verified using the expectation tests.
*   **Logging:** The project uses `easy_logging` for output.

Tests are defined in `lib/test.ml` and automatically use files in `examples/`. The testing framework is `ppx_expect`. If the output changes intentionally, you can promote the new output as the expected output using:

```bash
dune promote
```

## Development Conventions

*   **Language:** OCaml.
*   **Formatting:** The project uses `.ocamlformat` for code formatting.
*   **Testing:** New features should include corresponding example files in `examples/` and be verified using the expectation tests.
*   **Logging:** The project uses `easy_logging` for output.
