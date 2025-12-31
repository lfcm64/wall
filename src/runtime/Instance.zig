const Instance = @This();

const std = @import("std");
const llvm = @import("llvm");
const vm = @import("vm.zig");

const engine = llvm.engine;
const types = llvm.types;
const core = llvm.core;
const target = llvm.target;

const Allocator = std.mem.Allocator;

module: types.LLVMModuleRef,
eng: types.LLVMExecutionEngineRef,
vmctx: vm.VmCtx,

pub fn init(allocator: Allocator, module: types.LLVMModuleRef) !Instance {
    _ = target.LLVMInitializeNativeTarget();
    _ = target.LLVMInitializeNativeAsmPrinter();
    _ = target.LLVMInitializeNativeAsmParser();

    _ = engine.LLVMLinkInMCJIT();

    var error_msg: [*c]u8 = undefined;

    if (llvm.analysis.LLVMVerifyModule(
        module,
        types.LLVMVerifierFailureAction.LLVMReturnStatusAction,
        &error_msg,
    ) != 0) {
        std.debug.print("Module invalid: {s}\n", .{error_msg});
        defer core.LLVMDisposeMessage(error_msg);

        const module_str = core.LLVMPrintModuleToString(module);
        defer core.LLVMDisposeMessage(module_str);

        std.debug.print("{s}\n", .{module_str});
        return error.CompilationError;
    }

    var eng: types.LLVMExecutionEngineRef = undefined;

    if (engine.LLVMCreateExecutionEngineForModule(&eng, module, &error_msg) != 0) {
        core.LLVMDisposeMessage(error_msg);
        return error.ExecutionEngineError;
    }

    const set_i32_fn = core.LLVMGetNamedFunction(module, "set_i32");
    if (set_i32_fn != null) {
        engine.LLVMAddGlobalMapping(eng, set_i32_fn, @constCast(&vm.set_i32));
    }
    return .{
        .module = module,
        .eng = eng,
        .vmctx = try vm.VmCtx.init(allocator),
    };
}

pub fn deinit(self: *Instance) void {
    engine.LLVMDisposeExecutionEngine(self.eng);
    self.vmctx.deinit();
}

pub fn getFunction(self: *Instance, comptime func_name: []const u8, comptime Fn: type) !Fn {
    const func_addr = engine.LLVMGetFunctionAddress(self.eng, @ptrCast(func_name));
    if (func_addr == 0) return error.FunctionNotFound;

    return @ptrFromInt(func_addr);
}
