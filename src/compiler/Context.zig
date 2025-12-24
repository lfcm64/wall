const Context = @This();

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

llvm_types: std.ArrayList(types.LLVMTypeRef) = .{},
llvm_funcs: std.ArrayList(types.LLVMValueRef) = .{},

imported_funcs: u32 = 0,

pub fn init(allocator: Allocator) Context {
    const context = core.LLVMContextCreate();
    const module = core.LLVMModuleCreateWithNameInContext("wasm", context);
    const intrinsics = Intrinsics.init(module, context);
    return .{
        .allocator = allocator,
        .llvm_module = module,
        .llvm_context = context,
        .intrinsics = intrinsics,
    };
}

pub fn deinit(self: *Context) void {
    core.LLVMDisposeModule(self.llvm_module);
    core.LLVMContextDispose(self.llvm_context);

    self.llvm_types.deinit(self.allocator);
    self.llvm_funcs.deinit(self.allocator);
}

pub fn addType(self: *Context, ty: types.LLVMTypeRef) !void {
    try self.llvm_types.append(self.allocator, ty);
}

pub fn addFunc(self: *Context, func: types.LLVMValueRef) !void {
    try self.llvm_funcs.append(self.allocator, func);
}
