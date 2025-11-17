const std = @import("std");
const types = @import("types.zig");

const io = std.io;

const ValType = types.ValType;
const Mutability = types.Mutability;
const ExprValue = types.ExprValue;

pub const GlobalType = struct {
    val_type: ValType,
    mut: Mutability,

    pub fn fromReader(reader: *io.Reader) !GlobalType {
        const val_type: ValType = @enumFromInt(try reader.takeByte());
        const mut: Mutability = @enumFromInt(try reader.takeByte());

        return .{
            .val_type = val_type,
            .mut = mut,
        };
    }
};

pub const Global = struct {
    type: GlobalType,
    expr: ExprValue,

    pub fn fromReader(reader: *io.Reader) !Global {
        const global_type = try GlobalType.fromReader(reader);

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
};
