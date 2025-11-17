const std = @import("std");
const types = @import("types.zig");
const opcode = @import("../opcode.zig");

const io = std.io;
const assert = std.debug.assert;

const ValType = types.ValType;

const Opcode = opcode.Opcode;

pub const FuncType = struct {
    params: []const ValType,
    results: []const ValType,

    pub fn fromReader(reader: *io.Reader) !FuncType {
        const tag = try reader.takeByte();
        assert(tag == 0x60);

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

pub const FuncLocal = struct {
    count: u32,
    val_type: ValType,

    pub fn fromReader(reader: *io.Reader) !FuncLocal {
        const count = try reader.takeLeb128(u32);
        const val_type: ValType = @enumFromInt(try reader.takeByte());

        return .{
            .count = count,
            .val_type = val_type,
        };
    }
};

pub const Code = []const Opcode;

pub const FuncBodyStart = struct {
    size: u32,
    local_count: u32,

    pub fn fromReader(reader: *io.Reader) !FuncBodyStart {
        const size = try reader.takeLeb128(u32);
        const local_count = try reader.takeLeb128(u32);

        return .{
            .size = size,
            .local_count = local_count,
        };
    }
};
