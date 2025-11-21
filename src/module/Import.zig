const Import = @This();

const std = @import("std");
const types = @import("types.zig");

const io = std.io;

const Table = types.Table;
const Memory = types.Memory;
const GlobalType = types.Global.Type;

module: []const u8 = undefined,
name: []const u8 = undefined,
kind: Kind,

pub const Kind = union(enum) {
    func: u32,
    table: Table,
    memory: Memory,
    global: GlobalType,
};

pub fn fromReader(reader: *io.Reader) !Import {
    const tag = try reader.takeByte();

    const module_size = try reader.takeLeb128(u32);
    const module = try reader.take(module_size);

    const name_size = try reader.takeLeb128(u32);
    const name = try reader.take(name_size);

    const kind: Kind = switch (tag) {
        0x00 => .{ .func = try reader.takeLeb128(u32) },
        0x01 => .{ .table = try Table.fromReader(reader) },
        0x02 => .{ .memory = try Memory.fromReader(reader) },
        0x03 => .{ .global = try GlobalType.fromReader(reader) },
        else => return error.A,
    };
    return .{
        .module = module,
        .name = name,
        .kind = kind,
    };
}
