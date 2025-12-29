const std = @import("std");

const Module = @import("Module.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer _ = gpa.deinit();

    const source = @embedFile("tests/fib.wasm");

    var mod = Module.init(allocator, source);
    defer mod.deinit();

    var instance = try mod.instantiate();
    defer instance.deinit();

    const fib = try instance.getFunction(
        "fib",
        *const fn (*anyopaque, i32) callconv(.c) i32,
    );

    const result = fib(undefined, 8);
    std.debug.print("fib(8) = {}\n", .{result});
}
