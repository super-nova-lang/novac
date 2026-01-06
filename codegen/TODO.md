# Codegen TODO

## High/Medium Priority
- [ ] More than 6 parameters (stack-based param passing)
- [ ] Optional/variadic parameters (AST exists; lower and call ABI)
- [ ] Real struct/enum lowering (emit aggregates and with-block methods)
- [ ] Method/member calls (resolve receivers instead of extern varargs)
- [ ] Better error handling (replace unsupported placeholders with proper errors)
- [ ] Fat-pointer strings (length-tracked strings with consistent ABI/runtime helpers; currently null-terminated)
- [ ] LLVM ptr API update (move off deprecated `ptr_type` usage)
- [ ] LLVM integration improvements (optimization, portability)
- [ ] Linking improvements (external library support)

## Backend Targets
- [ ] Flesh out hand-rolled AMD64 backend
	- [ ] Prologue/epilogue emission helpers (stack frame setup/teardown)
	- [ ] Basic instruction selection for arithmetic/relational ops
	- [ ] Control flow: labels/branches for if/while/for/match lowering
	- [ ] Data layout for structs/enums (field offsets, tagging strategy)
	- [ ] String/constant emission into `.rodata`
	- [ ] Syscall/stdlib surface for `_start` and runtime hooks
- [ ] Align LLVM backend outputs with runtime/ABI once fat pointers return

## Finished
- [x] File layout: `.text`, `.data`, symbol visibility, section ordering
- [x] Calling convention: argument passing, return values, stack alignment, callee-saved regs
