const Export = @This();

const std = @import("std");
const types = @import("types.zig");

const io = std.io;

pub const Kind = union(enum) {
    func: u32,
    table: u32,
    memory: u32,
    global: u32,
};

name: []const u8,
kind: Kind,

pub fn fromReader(reader: *io.Reader) !Export {
    const name_size = try reader.takeLeb128(u32);
    const name: []const u8 = @ptrCast(try reader.take(name_size));

    const tag = try reader.takeByte();
    const index = try reader.takeLeb128(u32);

    const kind: Kind = switch (tag) {
        0x00 => .{ .func = index },
        0x01 => .{ .table = index },
        0x02 => .{ .memory = index },
        0x03 => .{ .global = index },
        else => return error.A,
    };
    return .{
        .name = name,
        .kind = kind,
    };
}
