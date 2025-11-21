const std = @import("std");
const types = @import("types.zig");

const io = std.io;

const Vec = types.primitives.Vec;
const VarU32 = types.primitives.VarU32;

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

pub const SectionType = enum(u8) {
    custom = 0x00,
    type = 0x01,
    import = 0x02,
    function = 0x03,
    table = 0x04,
    memory = 0x05,
    global = 0x06,
    @"export" = 0x07,
    start = 0x08,
    element = 0x09,
    code = 0x0a,
    data = 0x0b,
};

pub fn Section(comptime section_type: SectionType) type {
    return switch (section_type) {
        .custom => CustomSection,
        .type => Vec(types.Function.Type),
        .import => Vec(types.Import),
        .function => Vec(VarU32),
        .table => Vec(types.Table),
        .memory => Vec(types.Memory),
        .global => Vec(types.Global),
        .@"export" => Vec(types.Export),
        .start => VarU32,
        .element => Vec(types.Element),
        .code => Vec(types.Function),
        .data => Vec(types.Segment),
    };
}

pub const SectionHeader = struct {
    type: SectionType,
    size: u32,

    pub fn fromReader(reader: *io.Reader) !SectionHeader {
        const section_type: SectionType = @enumFromInt(try reader.takeByte());
        const size = try reader.takeLeb128(u32);

        return .{
            .type = section_type,
            .size = size,
        };
    }
};

const CustomSection = struct {
    name: []const u8,
    bytes: []const u8,

    pub fn fromBytes(bytes: []const u8) !CustomSection {
        var reader = io.Reader.fixed(bytes);

        const name_size = try reader.takeLeb128(u32);
        const name = try reader.take(name_size);

        const section_bytes_size = try reader.takeLeb128(u32);
        const section_bytes = try reader.take(section_bytes_size);

        return .{
            .name = name,
            .bytes = section_bytes,
        };
    }
};
