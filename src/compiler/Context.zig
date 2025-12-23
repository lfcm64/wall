const CompilerContext = @This();

const std = @import("std");
const llvm = @import("llvm");

const Intrinsics = @import("intrinsics.zig").Intrinsics;

const types = llvm.types;
const core = llvm.core;

const Allocator = std.mem.Allocator;

allocator: Allocator,

llvm_module: types.LLVMModuleRef,
llvm_context: types.LLVMContextRef,

intrinsics: Intrinsics,

llvm_functions: std.ArrayList(types.LLVMValueRef)

pub fn init() CompilerContext {
    const module = core.LLVMModuleCreateWithName("wasm");
    const context = core.LLVMContextCreate();
    const intrinsics = Intrinsics.init(module, context);
    return .{
        .llvm_module = module,
        .llvm_context = context,
        .intrinsics = intrinsics,
    };
}

pub fn deinit(self: *CompilerContext) void {
    core.LLVMContextDispose(self.llvm_context);
    core.LLVMDisposeModule(self.llvm_module);
}
