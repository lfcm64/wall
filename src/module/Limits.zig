const Limits = @This();

const std = @import("std");

const io = std.io;

min: u32,
max: ?u32 = null,

pub fn fromReader(reader: *io.Reader) !Limits {
    const tag = try reader.takeByte();
    const min = try reader.takeLeb128(u32);

    return switch (tag) {
        0x00 => .{ .min = min },
        0x01 => .{
            .min = min,
            .max = try reader.takeLeb128(u32),
        },
        else => error.A,
    };
}
