const Context = @This();

const std = @import("std");
const llvm = @import("llvm");

const Intrinsics = @import("codegen/intrinsics.zig").Intrinsics;
const Stubs = @import("codegen/stubs.zig").Stubs;

const types = llvm.types;
const core = llvm.core;

const Allocator = std.mem.Allocator;

allocator: Allocator,

llvm_module: types.LLVMModuleRef,
llvm_context: types.LLVMContextRef,

intrinsics: Intrinsics,
stubs: Stubs,

functypes: std.ArrayList(types.LLVMTypeRef) = .{},
funcs: std.ArrayList(types.LLVMValueRef) = .{},

imported_funcs: u32 = 0,

pub fn init(allocator: Allocator) Context {
    const ctx = core.LLVMContextCreate();
    const module = core.LLVMModuleCreateWithNameInContext("", ctx);
    const intrinsics = Intrinsics.init(module, ctx);
    const stubs = Stubs.init(module, ctx);
    return .{
        .allocator = allocator,
        .llvm_module = module,
        .llvm_context = ctx,
        .intrinsics = intrinsics,
        .stubs = stubs,
    };
}

pub fn deinit(self: *Context) void {
    core.LLVMContextDispose(self.llvm_context);

    self.functypes.deinit(self.allocator);
    self.funcs.deinit(self.allocator);
}

pub fn addType(self: *Context, ty: types.LLVMTypeRef) !void {
    try self.functypes.append(self.allocator, ty);
}

pub fn addFunc(self: *Context, func: types.LLVMValueRef) !void {
    try self.funcs.append(self.allocator, func);
}
