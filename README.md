# Nova - Your Goto Functional Language

### What Is Nova?

Nova is a new programming language written in OCaml that aims to bridge the gap between traditional C-syntax based languages and functional languages like OCaml, Haskell, and Lisp.

Nova is a garbage-collected with an opt in allocator system akin to [Odin's context](https://www.gingerbill.org/article/2025/12/15/odins-most-misunderstood-feature-context/).

### Key Features

 - **Friendly Syntax:** Functional programming with a familiar feel
 - **Garbage-collected:** Garbage-collected with an opt-in allocator system
 - **Modern Tooling:** Build on Dune's amazing ecosystem
 - **Modern C Interop:** Built with [libffi](https://github.com/libffi/libffi) first in mind

### The Project

 - **Lexer & Parser**: Full implementation for Nova source files.

#### Installing

Make sure you have llvm-19 on your system

```bash
$ export LLVM_CONFIG=/usr/lib/llvm19/bin/llvm-config
$ opam install . --deps-only
```
```
```

### Get Involved

Explore the source code, check out our vision, follow the socials, and contribute to the compiler:

- **[Nova on GitHub](https://github.com/super-nova-lang/novac)**
- **[Nova on Twitter (X)](https://x.com/SuperNovaLang)**
- **[Nova's Wiki](https://wiki.nova.nerdcult.net/index.php/Main_Page)**
- **[Contact Nova](mailto:super.nova.compiler@gmail.com)**
