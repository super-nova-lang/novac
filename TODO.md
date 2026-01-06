# To-Do (split by crate)

- Language + type system: see `analysis/TODO.md`.
- Code generation + backend work: see `codegen/TODO.md`.
- Standard library work: see `stdlib/TODO.md`.
- Developer experience and shared tasks: tracked here.

## Developer Experience
- [ ] Better logging (source-aware errors, progress, build time hints)
- [ ] Testing framework expansion beyond examples
- [ ] Debugging support (symbols / source maps)

## Completed ✓
- [x] Enum payload access (payload fields on variants)
- [x] CLI `parse` prints AST nodes to stdout
- [x] Parser/token positions carry spans; parser updated
- [x] Codegen CLI runs end-to-end on showcase (stubbed lowering)
- [x] Added `test-compiler`/`tc` and `test-compiler-promote`/`tcp` commands with JSON expectations in `tests/.expected.json`
- [x] Add `Doc_comment` token to lexer
- [x] Generate docs from source
- [x] Basic lexer, parser, and AST
- [x] If/else statements with elif chains
- [x] While loops
- [x] For loops
- [x] Match expressions (basic - literals and wildcards)
- [x] Relational operators (==, !=, <, >, <=, >=)
- [x] Arithmetic operators (+, -, *, /, %, ^)
- [x] Power operator (^) with integer exponentiation
- [x] Struct construction and field access
- [x] Enum construction (basic)
- [x] Function calls (up to 6 params)
- [x] Named call parameters in codegen (reorders by callee signature)
- [x] Implicit member lookups (receiver `.field` plus `.ptr`/`.type` reflection)
- [x] Type reflection via `.type` (returns type name value)
- [x] String literals with proper escaping
- [x] Parse command in CLI for syntax validation
- [x] Match enhancements (enum patterns, destructuring/tuples, guards)
- [x] Test harness improvements (line-aware, colorized diffs; expectations refreshed)
