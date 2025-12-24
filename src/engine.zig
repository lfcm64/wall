const std = @import("std");
const llvm = @import("llvm");

const engine = llvm.engine;
const types = llvm.types;
const core = llvm.core;

pub const Runtime = struct {
    eng: types.LLVMExecutionEngineRef,

    pub fn init(module: types.LLVMModuleRef) !@This() {
        _ = engine.LLVMLinkInMCJIT();

        var eng: types.LLVMExecutionEngineRef = undefined;
        var error_msg: [*c]u8 = undefined;

        if (engine.LLVMCreateExecutionEngineForModule(&eng, module, &error_msg) != 0) {
            core.LLVMDisposeMessage(error_msg);
            return error.ExecutionEngineError;
        }
        return .{ .eng = eng };
    }

    pub fn deinit(self: *@This()) void {
        engine.LLVMDisposeExecutionEngine(self.eng);
    }

    pub fn getFn(self: *@This(), comptime func_name: []const u8, comptime Fn: type) !Fn {
        const func_addr = engine.LLVMGetFunctionAddress(self.eng, @ptrCast(func_name));
        if (func_addr == 0) {
            return error.FunctionNotFound;
        }
        return @ptrFromInt(func_addr);
    }
};
