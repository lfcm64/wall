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

    const source = @embedFile("tests/add.wasm");

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
        return;
    }

    //const module_str = core.LLVMPrintModuleToString(compiler.ctx.llvm_module);
    //defer core.LLVMDisposeMessage(module_str);
    //std.debug.print("=== Module IR ===\n{s}\n", .{module_str});

    var runtime = try Runtime.init(compiler.ctx.llvm_module);

    const AddFn = *const fn (*anyopaque, i32, i32) callconv(.c) i32;

    const add_fn = try runtime.getFn("add", AddFn);

    const vmctx: *anyopaque = undefined;

    const result = add_fn(vmctx, 10, 5);
    std.debug.print("add(10, 5) = {}\n", .{result});
}
