# To-Do

## High Priority

### Language Features
	- [ ] **Derive/Trait system**
		- [ ] Derive attribute expansion
		- [ ] Trait definitions and implementations

### Type System
	- [ ] **Compile-time type inference** - Reduce explicit type annotations
	- [ ] **Better type checking** - Complete the analysis pass

### Standard Library
	- [ ] **Replace function stubs** - Current stdlib calls abort()
	- [ ] **Core data structures** - Collections, strings, I/O
	- [ ] **Math functions** - Beyond basic arithmetic

## Medium Priority

### Codegen Improvements
	- [ ] **More than 6 parameters** - Stack-based parameter passing
	- [ ] **Optional/variadic parameters** - AST exists, needs codegen
	- [ ] **Real struct/enum lowering** - remove zero-value fallbacks; emit aggregates and with-block methods
	- [ ] **Method/member calls** - resolve receivers (e.g., `p1.greet`) instead of treating as extern varargs
	- [ ] **Better error handling** - Replace unsupported placeholders with proper errors
	- [ ] **Fat-pointer strings** - Reintroduce length-tracked strings (fat pointers) with consistent ABI and runtime helpers; currently using null-terminated C strings for stability.
	- [ ] **LLVM ptr API update** - Move off deprecated `ptr_type` usage to `Context::ptr_type`

### Build System
	- [ ] **LLVM integration** - Better optimization and portability
	- [ ] **Linking improvements** - External library support

## Low Priority

### Developer Experience
	- [ ] **Better logging**
		- [ ] Descriptive error messages with source locations
		- [ ] Build time estimation
		- [ ] Progress indicators
	- [ ] **Testing framework** - Expand beyond current examples
	- [ ] **Debugging support** - Source maps, debug symbols

## Completed ✓
	- [x] Enum payload access (payload fields on variants)
	- [x] CLI `parse` prints AST nodes to stdout
	- [x] Parser/token positions - tokens carry spans; parser updated to new token API
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
