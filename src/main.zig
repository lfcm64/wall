const std = @import("std");

const Parser = @import("parser/Parser.zig");
const Module = @import("Module.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer _ = gpa.deinit();

    const source = @embedFile("tests/fib.wasm");
    var parser = Parser.init(source);

    var module = Module.init(allocator);
    defer module.deinit();

    try module.parse(&parser);
    try module.validate();
}
