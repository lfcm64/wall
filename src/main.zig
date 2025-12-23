const std = @import("std");

const Parser = @import("parser/Parser.zig");
const Validator = @import("validation/Validator.zig");

const Pipeline = @import("pipeline.zig").Pipeline;

const ValidationPipeline = Pipeline(&[_]type{*Validator});

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer _ = gpa.deinit();

    const source = @embedFile("tests/fib.wasm");

    var parser = Parser.init(source);

    var validator = Validator.init(allocator);
    defer validator.deinit();

    var pipeline = ValidationPipeline.init(.{&validator});

    try pipeline.run(&parser);
}
