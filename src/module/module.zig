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

pub const SectionType = enum(u8) {
    Custom = 0x00,
    Type = 0x01,
    Import = 0x02,
    Function = 0x03,
    Table = 0x04,
    Memory = 0x05,
    Global = 0x06,
    Export = 0x07,
    Start = 0x08,
    Element = 0x09,
    Code = 0x0a,
    Data = 0x0b,
};

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

pub const CustomSection = struct {
    name: []const u8,
    bytes: []const u8,

    pub fn fromReader(reader: *io.Reader) !CustomSection {
        const name_size = try reader.takeLeb128(u32);
        const name = try reader.take(name_size);

        const bytes_size = try reader.takeLeb128(u32);
        const bytes = try reader.take(bytes_size);

        return .{
            .name = name,
            .bytes = bytes,
        };
    }
};

pub const StartSection = struct {
    start: u32,

    pub fn fromReader(reader: *io.Reader) !StartSection {
        return .{ .start = try reader.takeInt(u32, .little) };
    }
};
