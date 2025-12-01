const std = @import("std");
const clap = @import("clap");

const Parser = @import("parser/Parser.zig");

const Allocator = std.mem.Allocator;

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
        .err => "\x1b[31m", // red
        .warn => "\x1b[33m", // yellow
        .info => "\x1b[36m", // cyan
        .debug => "\x1b[34m", // blue
    };
    var buffer: [64]u8 = undefined;
    const stderr = std.debug.lockStderrWriter(&buffer);
    defer std.debug.unlockStderrWriter();
    nosuspend stderr.print(color ++ level_txt ++ prefix2 ++ fmt ++ reset ++ "\n", args) catch return;
}

pub const std_options = std.Options{
    .logFn = log,
    .log_level = .info,
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    const params = comptime clap.parseParamsComptime(
        \\-h, --help        Show this help.
        \\-v, --validate <str>   Validate a Wasm module.
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

    if (res.args.help != 0)
        std.debug.print("--help\n", .{});
    if (res.args.validate) |file_path|
        try validateFile(gpa.allocator(), file_path);
}

fn validateFile(allocator: Allocator, file_path: []const u8) !void {
    const file = try std.fs.cwd().openFile(file_path, .{ .mode = .read_only });

    const buf = try file.readToEndAlloc(allocator, 999_999);
    defer allocator.free(buf);

    var parser = Parser.init(allocator, buf);
    defer parser.deinit();

    while (try parser.parseNext()) |_| {}
}
