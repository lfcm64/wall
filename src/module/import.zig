const std = @import("std");
const types = @import("types.zig");

const io = std.io;
const assert = std.debug.assert;

const Table = types.table.Table;
const Memory = types.Memory;
const GlobalType = types.global.GlobalType;

pub const ImportKind = union(enum) {
    func: u32,
    table: Table,
    memory: Memory,
    global: GlobalType,
};

pub const Import = struct {
    module: []const u8 = undefined,
    name: []const u8 = undefined,
    kind: ImportKind,

    pub fn fromReader(reader: *io.Reader) !Import {
        const tag = try reader.takeByte();

        const module_size = try reader.takeLeb128(u32);
        const module = try reader.take(module_size);

        const name_size = try reader.takeLeb128(u32);
        const name = try reader.take(name_size);

        const kind = switch (tag) {
            0x00 => ImportKind{ .func = try reader.takeLeb128(u32) },
            0x01 => ImportKind{ .table = try Table.fromReader(reader) },
            0x02 => ImportKind{ .memory = try Memory.fromReader(reader) },
            0x03 => ImportKind{ .global = try GlobalType.fromReader(reader) },
            else => return error.A,
        };
        return .{
            .module = module,
            .name = name,
            .kind = kind,
        };
    }
};
