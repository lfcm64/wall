const Global = @This();

const std = @import("std");
const types = @import("types.zig");

const io = std.io;

const ValType = types.ValType;
const ExprValue = types.Expr.Value;

type: Type,
expr: ExprValue,

pub const Mutability = enum(u8) {
    @"const" = 0x00,
    @"var" = 0x01,
};

pub const Type = struct {
    val_type: ValType,
    mut: Mutability,

    pub fn fromReader(reader: *io.Reader) !Type {
        const val_type: ValType = @enumFromInt(try reader.takeByte());
        const mut: Mutability = @enumFromInt(try reader.takeByte());

        return .{
            .val_type = val_type,
            .mut = mut,
        };
    }
};

pub fn fromReader(reader: *io.Reader) !Global {
    const global_type = try Type.fromReader(reader);

    const expr = switch (global_type.val_type) {
        .i32 => ExprValue{ .i32 = try reader.takeLeb128(i32) },
        .i64 => ExprValue{ .i64 = try reader.takeLeb128(i32) },
        .f32 => ExprValue{ .f32 = @floatFromInt(try reader.takeInt(u32, .little)) },
        .f64 => ExprValue{ .f64 = @floatFromInt(try reader.takeInt(u64, .little)) },
    };
    return .{
        .type = global_type,
        .expr = expr,
    };
}
