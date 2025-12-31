const std = @import("std");
const llvm = @import("llvm");
const wasm = @import("wasm");

const core = llvm.core;
const types = llvm.types;

const ValType = wasm.types.ValType;
const BlockType = wasm.instr.BlockType;

pub fn typeToLLVM(ty: ValType, context: types.LLVMContextRef) types.LLVMTypeRef {
    return switch (ty) {
        .i32 => core.LLVMInt32TypeInContext(context),
        .i64 => core.LLVMInt64TypeInContext(context),
        .f32 => core.LLVMFloatTypeInContext(context),
        .f64 => core.LLVMDoubleTypeInContext(context),
    };
}

pub fn blockTypeToPhi(block_type: BlockType, context: types.LLVMContextRef, builder: types.LLVMBuilderRef) ?types.LLVMValueRef {
    const val_ty = if (block_type == .valtype) block_type.valtype else return null;

    const llvm_ty = switch (val_ty) {
        .i32 => core.LLVMInt32TypeInContext(context),
        .i64 => core.LLVMInt64TypeInContext(context),
        .f32 => core.LLVMFloatTypeInContext(context),
        .f64 => core.LLVMDoubleTypeInContext(context),
    };
    return core.LLVMBuildPhi(builder, llvm_ty, "");
}
