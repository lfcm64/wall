const std = @import("std");
const types = @import("types.zig");
const opcode = @import("../opcode.zig");

const io = std.io;
const assert = std.debug.assert;

const ValType = types.ValType;
const Section = types.sections.Section;

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

pub const FuncBody = struct {
    size: u32,
    locals: Section(FuncLocal),
    code: []const u8,

    pub fn fromReader(reader: *io.Reader) !FuncBody {
        const size = try reader.takeLeb128(u32);

        const initial_pos = reader.seek;
        const local_count = try reader.takeLeb128(u32);

        for (0..local_count) |_| {
            _ = try FuncLocal.fromReader(reader);
        }
        const bytes = reader.buffer[initial_pos..reader.seek];
        const locals = try Section(FuncLocal).fromBytes(bytes);

        const code_size = size - (reader.seek - initial_pos);
        const code = try reader.take(code_size);

        return .{
            .size = size,
            .locals = locals,
            .code = code,
        };
    }
};
