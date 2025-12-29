const std = @import("std");

const llvm = @import("llvm");
const wasm = @import("../../wasm/wasm.zig");
const conv = @import("conv.zig");

const Context = @import("../Context.zig");

const types = llvm.types;
const core = llvm.core;
const indices = wasm.indices;

pub const FunctionTypeCompiler = struct {
    pub fn compile(ctx: *Context, func_type: wasm.types.FuncType, _: u32) !void {
        const param_count = func_type.params.len;

        const param_types = try ctx.allocator.alloc(types.LLVMTypeRef, param_count + 1);
        defer ctx.allocator.free(param_types);

        param_types[0] = core.LLVMPointerTypeInContext(ctx.llvm_context, 0);
        for (0..param_count) |i| {
            param_types[i + 1] = conv.typeToLLVM(func_type.params[i], ctx.llvm_context);
        }

        if (func_type.results.len > 1) @panic("compiler does not support multi return function");
        const return_type = switch (func_type.results.len) {
            0 => core.LLVMVoidTypeInContext(ctx.llvm_context),
            else => conv.typeToLLVM(func_type.results[0], ctx.llvm_context),
        };

        const llvm_func_type = core.LLVMFunctionType(
            return_type,
            param_types.ptr,
            @intCast(param_types.len),
            0,
        );
        try ctx.addType(llvm_func_type);
    }
};

pub const FunctionCompiler = struct {
    pub fn compile(ctx: *Context, type_idx: indices.Func, _: u32) !void {
        const func_type = ctx.llvm_types.items[type_idx];

        const func = core.LLVMAddFunction(ctx.llvm_module, "", func_type);
        core.LLVMSetLinkage(func, types.LLVMLinkage.LLVMInternalLinkage);

        try ctx.addFunc(func);
    }
};
