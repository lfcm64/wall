const std = @import("std");
const clap = @import("clap");

const Parser = @import("parser/Parser.zig");

fn log(
    comptime level: std.log.Level,
    comptime scope: @Type(.enum_literal),
    comptime fmt: []const u8,
    args: anytype,
) void {
    const level_txt = comptime level.asText();
    const prefix2 = if (scope == .default) ": " else "(" ++ @tagName(scope) ++ "): ";
    const reset = "\x1b[0m";

    const color = switch (level) {
        .err => "",
        .warn => "",
        .info => "\x1b[36m",
        .debug => "\x1b[34m",
    };
    var buffer: [64]u8 = undefined;
    const stderr = std.debug.lockStderrWriter(&buffer);
    defer std.debug.unlockStderrWriter();
    nosuspend stderr.print(color ++ level_txt ++ prefix2 ++ fmt ++ reset ++ "\n", args) catch return;
}

pub const std_options = std.Options{
    .logFn = log,
};

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

        while (try p.parseNext()) |_| {}
    }
}
