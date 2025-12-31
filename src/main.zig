const std = @import("std");

const Module = @import("Module.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer _ = gpa.deinit();

    const source = @embedFile("tests/test.wasm");

    var module = Module.init(allocator, source);
    defer module.deinit();

    var instance = try module.instantiate();
    defer instance.deinit();

    const writeZero = try instance.getFunction(
        "writeZero",
        *const fn (*anyopaque) callconv(.c) void,
    );

    std.debug.print("{any}\n", .{instance.vmctx.memory});
    _ = writeZero(&instance.vmctx);

    std.debug.print("{any}\n", .{instance.vmctx.memory});
    //std.debug.print("fib(8) = {}\n", .{result});
}
