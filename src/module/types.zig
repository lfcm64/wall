const std = @import("std");
const types = @import("types.zig");

const io = std.io;

pub const ValType = enum(u8) {
    i32 = 0x7f,
    i64 = 0x7e,
    f32 = 0x7d,
    f64 = 0x7c,
};

pub const ExprValue = union(ValType) {
    i32: i32,
    i64: i64,
    f32: f32,
    f64: f64,
};

pub const Expr = struct {
    value: ExprValue,
    global_idx: u32,
};

pub const Mutability = enum(u8) {
    @"const" = 0x00,
    @"var" = 0x01,
};

pub const Limits = struct {
    min: u32,
    max: ?u32 = null,

    pub fn fromReader(reader: *io.Reader) !Limits {
        const tag = try reader.takeByte();
        const min = try reader.takeLeb128(u32);

        return switch (tag) {
            0x00 => .{ .min = min },
            0x01 => .{
                .min = min,
                .max = try reader.takeLeb128(u32),
            },
            else => error.A,
        };
    }
};

pub const Memory = Limits;

pub const VarU32 = struct {
    val: u32,

    pub fn fromReader(reader: *io.Reader) !VarU32 {
        return .{ .val = try reader.takeLeb128(u32) };
    }
};

pub const element = @import("element.zig");
pub const @"export" = @import("export.zig");
pub const function = @import("function.zig");
pub const global = @import("global.zig");
pub const import = @import("import.zig");
pub const module = @import("module.zig");
pub const segment = @import("segment.zig");
pub const table = @import("table.zig");
