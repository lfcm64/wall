# wall

A WebAssembly **runtime** + **aot compiler** project.

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

## Basic usage

Validate a `.wasm` file:

```
wall validate file.wasm
```

## Roadmap

* Add LLVM IR generation
* Add runtime (memory, tables, imports)
* Add codegen


