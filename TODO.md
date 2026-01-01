# To-Do

## Completed ✓
  - [x] Basic lexer, parser, and AST
  - [x] If/else statements with elif chains
  - [x] While loops
  - [x] Match expressions (basic - literals and wildcards)
  - [x] Relational operators (==, !=, <, >, <=, >=)
  - [x] Arithmetic operators (+, -, *, /, %)
  - [x] Struct construction and field access
  - [x] Enum construction (basic)
  - [x] Function calls (up to 6 params)
  - [x] String literals with proper escaping

## High Priority

### Language Features
  - [ ] **Enum payload access** - Access fields in enum variant payloads
  - [ ] **Match enhancements**
    - [ ] Enum variant patterns
    - [ ] Destructuring patterns
    - [ ] Tuple patterns
    - [ ] Complex guard expressions
  - [ ] **For loops** - Iteration over collections
  - [ ] **Power operator** (`**`) - AST exists, needs codegen
  - [ ] **Macro system**
    - [ ] Macro expansion
    - [ ] Macro execution at compile-time
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
  - [ ] **Named call parameters** - `func(~name: value)` syntax
  - [ ] **More than 6 parameters** - Stack-based parameter passing
  - [ ] **Optional/variadic parameters** - AST exists, needs codegen
  - [ ] **Implicit member lookups** - Full support for `.field` syntax
  - [ ] **Better error handling** - Replace unsupported placeholders with proper errors

### Build System
  - [ ] **LLVM integration** - Better optimization and portability
  - [ ] **Linking improvements** - External library support

## Low Priority

### Developer Experience
  - [ ] **Better logging**
    - [ ] Descriptive error messages with source locations
    - [ ] Build time estimation
    - [ ] Progress indicators
  - [ ] **Documentation generation**
    - [ ] Add `Doc_comment` token to lexer
    - [ ] Generate docs from source
  - [ ] **Testing framework** - Expand beyond current examples
  - [ ] **Debugging support** - Source maps, debug symbols
