const std = @import("std");
const types = @import("types.zig");

const io = std.io;

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

pub fn Section(comptime Item: type) type {
    return struct {
        bytes: []const u8,
        count: u32,
        item_offset: u32,

        const Self = @This();

        pub fn fromBytes(bytes: []const u8) !Self {
            var reader = io.Reader.fixed(bytes);
            const count = try reader.takeLeb128(u32);

            return .{
                .bytes = bytes,
                .count = count,
                .item_offset = @intCast(reader.seek),
            };
        }

        pub fn iter(self: *const Self) Iterator {
            const bytes = self.bytes[self.item_offset..];
            return Iterator{
                .reader = io.Reader.fixed(bytes),
                .count = self.count,
            };
        }

        const Iterator = struct {
            reader: io.Reader,
            count: u32,
            index: u32 = 0,

            pub fn next(it: *Iterator) !?Item {
                if (it.index == it.count) return null;
                const item = try Item.fromReader(&it.reader);
                it.index += 1;
                return item;
            }

            pub fn isFinished(it: *Iterator) bool {
                return it.reader.seek == it.reader.buffer.len and it.count == it.index;
            }
        };
    };
}

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

pub const TypeSection = Section(types.function.FuncType);
pub const ImportSection = Section(types.import.Import);
pub const FunctionSection = Section(types.VarU32);
pub const TableSection = Section(types.table.Table);
pub const MemorySection = Section(types.Memory);
pub const GlobalSection = Section(types.global.Global);
pub const ExportSection = Section(types.@"export".Export);
pub const DataSection = Section(types.segment.DataSegment);

pub const StartSection = struct {
    start: u32,

    pub fn fromReader(reader: *io.Reader) !StartSection {
        return .{ .start = try reader.takeInt(u32, .little) };
    }
};

pub const ElementSection = Section(types.element.Element);
pub const CodeSection = Section(types.function.FuncBody);
