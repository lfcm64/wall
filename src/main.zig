const std = @import("std");
const clap = @import("clap");

const Parser = @import("parser/Parser.zig");

pub fn main() !void {
    var gpa = std.heap.DebugAllocator(.{}){};
    defer _ = gpa.deinit();

    const params = comptime clap.parseParamsComptime(
        \\-h, --help                Display this help and exit.
        \\-v, --validate            Validate a Wasm module.
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

    if (res.args.help != 0)
        std.debug.print("--help\n", .{});
    if (res.args.validate != 0) {
        var p = Parser.init(gpa.allocator(), source);
        defer p.deinit();

        while (try p.next()) |_| {}
    }
}
