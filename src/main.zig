const std = @import("std");
const llvm = @import("llvm");

const core = llvm.core;
const types = llvm.types;
const target = llvm.target;

const Parser = @import("parser/Parser.zig");
const Validator = @import("validation/Validator.zig");
const Compiler = @import("compiler/Compiler.zig");

const Runtime = @import("engine.zig").Runtime;

const Pipeline = @import("pipeline.zig").Pipeline;

const ValidationPipeline = Pipeline(&[_]type{ *Validator, *Compiler });

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer _ = gpa.deinit();

    const source = @embedFile("tests/fib.wasm");

    var parser = Parser.init(source);

    var validator = Validator.init(allocator);
    defer validator.deinit();

    _ = target.LLVMInitializeNativeTarget();
    _ = target.LLVMInitializeNativeAsmPrinter();
    _ = target.LLVMInitializeNativeAsmParser();

    var compiler = Compiler.init(allocator);
    defer compiler.deinit();

    var pipeline = ValidationPipeline.init(.{ &validator, &compiler });

    try pipeline.run(&parser);

    var error_msg: [*c]u8 = undefined;
    if (llvm.analysis.LLVMVerifyModule(
        compiler.ctx.llvm_module,
        types.LLVMVerifierFailureAction.LLVMReturnStatusAction,
        &error_msg,
    ) != 0) {
        defer core.LLVMDisposeMessage(error_msg);
        std.debug.print("Empty module invalid: {s}\n", .{error_msg});

        const module_str = core.LLVMPrintModuleToString(compiler.ctx.llvm_module);
        defer core.LLVMDisposeMessage(module_str);
        std.debug.print("=== Module IR ===\n{s}\n", .{module_str});

        return;
    }

    var runtime = try Runtime.init(compiler.ctx.llvm_module);

    const FibFn = *const fn (*anyopaque, i32) callconv(.c) i32;

    const fib = try runtime.getFn("fib", FibFn);

    const vmctx: *anyopaque = undefined;

    const result = fib(vmctx, 20);
    std.debug.print("fib(20) = {}\n", .{result});
}

