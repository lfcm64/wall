const std = @import("std");
const types = @import("types.zig");

const io = std.io;

const Expr = types.Expr;

pub const ElementStart = struct {
    table_idx: u32,
    offset: Expr,
    index_count: u32,

    pub fn fromReader(reader: *io.Reader) !ElementStart {
        const table_idx = try reader.takeLeb128(u32);
        const offset = Expr{
            .value = .{ .i32 = try reader.takeLeb128(i32) },
            .global_idx = try reader.takeLeb128(u32),
        };
        const index_count = try reader.takeLeb128(u32);

        return .{
            .table_idx = table_idx,
            .offset = offset,
            .index_count = index_count,
        };
    }
};
