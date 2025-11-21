const Function = @This();

const std = @import("std");
const types = @import("types.zig");

const io = std.io;

const ValType = types.ValType;
const Vec = types.primitives.Vec;

size: u32,
locals: Vec(Local),
code: []const u8,

pub const Type = struct {
    params: []const ValType,
    results: []const ValType,

    pub fn fromReader(reader: *io.Reader) !Type {
        const tag = try reader.takeByte();
        if (tag != 0x60) return error.BadTag;

        const param_count = try reader.takeLeb128(u32);
        const params: []const ValType = @ptrCast(try reader.take(param_count));

        const result_count = try reader.takeLeb128(u32);
        const results: []const ValType = @ptrCast(try reader.take(result_count));

        return .{
            .params = params,
            .results = results,
        };
    }
};

pub const Local = struct {
    count: u32,
    val_type: ValType,

    pub fn fromReader(reader: *io.Reader) !Local {
        const count = try reader.takeLeb128(u32);
        const val_type: ValType = @enumFromInt(try reader.takeByte());

        return .{
            .count = count,
            .val_type = val_type,
        };
    }
};

pub fn fromReader(reader: *io.Reader) !Function {
    const size = try reader.takeLeb128(u32);

    const initial_pos = reader.seek;
    const local_count = try reader.takeLeb128(u32);

    for (0..local_count) |_| {
        _ = try Local.fromReader(reader);
    }
    const bytes = reader.buffer[initial_pos..reader.seek];
    const locals = try Vec(Local).fromBytes(bytes);

    const code_size = size - (reader.seek - initial_pos);
    const code = try reader.take(code_size);

    return .{
        .size = size,
        .locals = locals,
        .code = code,
    };
}
