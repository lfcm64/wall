const std = @import("std");
const types = @import("types.zig");

const io = std.io;

const Section = types.sections.Section;

pub const Element = struct {
    table_idx: u32,
    offset: types.Expr,
    indices: Section(types.VarU32),

    pub fn fromReader(reader: *io.Reader) !Element {
        const table_idx = try reader.takeLeb128(u32);
        const offset = types.Expr{
            .value = .{ .i32 = try reader.takeLeb128(i32) },
            .global_idx = try reader.takeLeb128(u32),
        };

        const initial_pos = reader.seek;
        const index_count = try reader.takeLeb128(u32);

        for (0..index_count) |_| {
            _ = try types.VarU32.fromReader(reader);
        }
        const bytes = reader.buffer[initial_pos..reader.seek];
        const indices = try Section(types.VarU32).fromBytes(bytes);

        return .{
            .table_idx = table_idx,
            .offset = offset,
            .indices = indices,
        };
    }
};
