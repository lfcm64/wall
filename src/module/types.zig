const std = @import("std");

const io = std.io;

pub const ModuleHeader = struct {
    magic: u32,
    version: u32,

    pub fn fromReader(reader: *io.Reader) !ModuleHeader {
        const magic = try reader.takeInt(u32, .little);
        const version = try reader.takeInt(u32, .little);

        return .{
            .magic = magic,
            .version = version,
        };
    }
};

pub const ValType = enum(u8) {
    i32 = 0x7f,
    i64 = 0x7e,
    f32 = 0x7d,
    f64 = 0x7c,
};

pub const Memory = Limits;

pub const Element = @import("Element.zig");
pub const Export = @import("Export.zig");
pub const Expr = @import("Expr.zig");
pub const Function = @import("Function.zig");
pub const Global = @import("Global.zig");
pub const Import = @import("Import.zig");
pub const Limits = @import("Limits.zig");
pub const Segment = @import("Segment.zig");
pub const Table = @import("Table.zig");

pub const module = @import("module.zig");
pub const primitives = @import("primitives.zig");
