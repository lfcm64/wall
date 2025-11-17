const std = @import("std");
const clap = @import("clap");
const pipeline = @import("pipeline.zig");

pub fn main() !void {
    var gpa = std.heap.DebugAllocator(.{}){};
    defer _ = gpa.deinit();

    const params = comptime clap.parseParamsComptime(
        \\-h, --help                Display this help and exit.
        \\-v, --validate <str>...   Validate a Wasm module.
    );

    var diag = clap.Diagnostic{};
    var res = clap.parse(clap.Help, &params, clap.parsers.default, .{
        .diagnostic = &diag,
        .allocator = gpa.allocator(),
    }) catch |err| {
        try diag.reportToFile(.stderr(), err);
        return err;
    };
    defer res.deinit();

    const source = @embedFile("tests/fib.wasm");

    var p = pipeline.FrontendPipeline.init(
        gpa.allocator(),
        source,
        .{ .print_ast = true },
    );

    try p.run();

    if (res.args.help != 0)
        std.debug.print("--help\n", .{});
    for (res.args.validate) |s|
        std.debug.print("--validate = {s}\n", .{s});
}
