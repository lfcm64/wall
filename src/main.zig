const std = @import("std");
const llvm = @import("llvm");

const core = llvm.core;
const types = llvm.types;
const target = llvm.target;

const Parser = @import("parser/Parser.zig");
const Validator = @import("validation/Validator.zig");
const Compiler = @import("compiler/Compiler.zig");

const Runtime = @import("runtime/Runtime.zig");

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

    var compiler = Compiler.init(allocator);
    defer compiler.deinit();

    var pipeline = ValidationPipeline.init(.{ &validator, &compiler });
    try pipeline.exec(&parser);

    var runtime = try Runtime.init(compiler.ctx.llvm_module);

    const fib = try runtime.getFn("fib", *const fn (*anyopaque, i32) callconv(.c) i32);

    const result = fib(undefined, 8);
    std.debug.print("fib(8) = {}\n", .{result});
}
