const std = @import("std");

const Parser = @import("parser/Parser.zig");
const Validator = @import("validation/Validator.zig");

const Context = @import("parser/Context.zig");
const Pipeline = @import("pipeline.zig").Pipeline;

const ValidationPipeline = Pipeline(&[_]type{Validator});

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer _ = gpa.deinit();

    const source = @embedFile("tests/fib.wasm");

    var ctx = Context.init(allocator);
    defer ctx.deinit();

    var parser = Parser.init(source, &ctx);

    const validator = Validator.init(.{});
    var pipeline = ValidationPipeline.init(.{validator});

    try pipeline.run(&parser);
}
