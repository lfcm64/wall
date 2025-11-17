const std = @import("std");
const types = @import("types.zig");

const io = std.io;

pub const ExportKind = union(enum) {
    func: u32,
    table: u32,
    memory: u32,
    global: u32,
};

pub const Export = struct {
    name: []const u8,
    kind: ExportKind,

    pub fn fromReader(reader: *io.Reader) !Export {
        const name_size = try reader.takeLeb128(u32);
        const name: []const u8 = @ptrCast(try reader.take(name_size));

        const tag = try reader.takeByte();
        const index = try reader.takeLeb128(u32);

        const kind = switch (tag) {
            0x00 => ExportKind{ .func = index },
            0x01 => ExportKind{ .table = index },
            0x02 => ExportKind{ .memory = index },
            0x03 => ExportKind{ .global = index },
            else => return error.A,
        };
        return .{
            .name = name,
            .kind = kind,
        };
    }
};
