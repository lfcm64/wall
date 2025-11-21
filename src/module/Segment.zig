const Segment = @This();

const std = @import("std");
const types = @import("types.zig");

const io = std.io;

const Expr = types.Expr;

memory_idx: u32,
offset: Expr,
bytes: []const u8,

pub fn fromReader(reader: *io.Reader) !Segment {
    const memory_idx = try reader.takeLeb128(u32);

    const offset = Expr{
        .value = .{ .i32 = try reader.takeLeb128(i32) },
        .global_idx = try reader.takeLeb128(u32),
    };

    const bytes_size = try reader.takeLeb128(u32);
    const bytes: []const u8 = @ptrCast(try reader.take(bytes_size));

    return .{
        .memory_idx = memory_idx,
        .offset = offset,
        .bytes = bytes,
    };
}
