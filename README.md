# wall

A WebAssembly **runtime** + **JIT compiler** project.

Currently **only the parser and validator are implemented**.

## Goal

Provide a small, clean foundation for:

* Parsing Wasm binaries
* Validating modules
* (Future) Converting Wasm to LLVM IR
* (Future) Running Wasm modules

## Status

* ✔ Parser (section-by-section)
* ✔ Validator
* ❌ LLVM IR generation
* ❌ Runtime
* ❌ Codegen / AOT

## Build

```
zig build
```

## Roadmap

* Add LLVM IR generation
* Add runtime (memory, tables, imports)
* Add codegen
