const std = @import("std");
const llvm = @import("llvm");
const wasm = @import("../../wasm/wasm.zig");

const llvm_types = llvm.types;
const core = llvm.core;

const wasm_types = wasm.types;

pub fn wasmToLLVMTypeInContext(ty: wasm_types.ValType, context: llvm_types.LLVMContextRef) llvm_types.LLVMTypeRef {
    return switch (ty) {
        .i32 => core.LLVMInt32TypeInContext(context),
        .i64 => core.LLVMInt64TypeInContext(context),
        .f32 => core.LLVMFloatTypeInContext(context),
        .f64 => core.LLVMDoubleTypeInContext(context),
    };
}
