const std = @import("std");

const llvm = @import("llvm");
const wasm = @import("../../wasm/wasm.zig");
const conv = @import("conv.zig");

const Context = @import("../Context.zig");

const types = llvm.types;
const core = llvm.core;

pub const ExportCompiler = struct {
    pub fn compile(ctx: *Context, exp: wasm.types.Export, _: u32) !void {
        switch (exp.kind) {
            .func => |idx| {
                const func = ctx.llvm_funcs.items[idx];
                core.LLVMSetValueName2(func, @ptrCast(exp.name), @intCast(exp.name.len));
                core.LLVMSetLinkage(func, types.LLVMLinkage.LLVMExternalLinkage);
            },
            else => @panic("only function support exporting for now"),
        }
    }
};
