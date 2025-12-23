const std = @import("std");

const llvm = @import("llvm");
const wasm = @import("../../wasm/wasm.zig");
const conv = @import("conv.zig");

const Context = @import("../Context.zig");

const types = llvm.types;
const core = llvm.core;

const Allocator = std.mem.Allocator;

pub const FunctionInfo = struct {
    ty: wasm.types.FuncType,
    kind: FunctionKind,
    exported: ?wasm.types.Export,
};

pub const FunctionKind = union(enum) {
    imported: wasm.types.Import,
    defined: wasm.types.FuncBody,
};

pub const FunctionCompiler = struct {
    pub fn compile(ctx: *Context, func_info: FunctionInfo) !types.LLVMValueRef {
        const func_type = llvmFuncType(ctx, func_info);

        const func_name = if (func_info.exported) |exp| exp.name else "";
        const func = core.LLVMAddFunction(
            ctx.llvm_module,
            @ptrCast(func_name),
            func_type,
        );
    }

    fn llvmFuncType(ctx: *Context, func_info: FunctionInfo) types.LLVMTypeRef {
        const param_count = ctx.func_type.params.len;

        const param_types = try ctx.allocator.alloc(types.LLVMTypeRef, param_count + 1);
        errdefer ctx.allocator.free(param_types);

        param_types[0] = core.LLVMPointerTypeInContext(ctx.llvm_ctx, 0);
        for (1..param_count) |i| {
            param_types[i] = conv.wasmToLLVMTypeInContext(func_info.ty[i - 1], ctx.llvm_ctx);
        }

        if (func_info.ty.results.len > 1) @compileError("compiler does not support multi return function");
        const return_type = switch (func_info.ty.results.len) {
            0 => core.LLVMVoidTypeInContext(ctx.llvm_ctx),
            else => conv.wasmToLLVMTypeInContext(func_info.ty.results[0]),
        };

        return core.LLVMFunctionType(
            return_type,
            param_types.ptr,
            @intCast(param_types.len),
            0,
        );
    }
};
