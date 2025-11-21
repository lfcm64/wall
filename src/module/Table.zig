const Table = @This();

const std = @import("std");
const types = @import("types.zig");

const io = std.io;

const Limits = types.Limits;

limits: Limits,

pub fn fromReader(reader: *io.Reader) !Table {
    const tag = try reader.takeByte();
    if (tag != 0x70) return error.BadTag;

    return .{ .limits = try Limits.fromReader(reader) };
}
