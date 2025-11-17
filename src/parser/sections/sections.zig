const std = @import("std");
const types = @import("../../module/types.zig");
const streams = @import("streams.zig");

const io = std.io;

pub const TypeSection = IterableSection(types.function.FuncType);
pub const ImportSection = IterableSection(types.import.Import);
pub const FunctionSection = IterableSection(types.VarU32);
pub const TableSection = IterableSection(types.table.Table);
pub const MemorySection = IterableSection(types.Memory);
pub const GlobalSection = IterableSection(types.global.Global);
pub const ExportSection = IterableSection(types.@"export".Export);
pub const DataSection = IterableSection(types.segment.DataSegment);
pub const ElementSection = StreamableSection(streams.ElementStream);
pub const CodeSection = StreamableSection(streams.CodeStream);

pub fn IterableSection(comptime Item: type) type {
    return struct {
        bytes: []const u8,
        count: u32,

        const Self = @This();

        pub fn fromBytes(bytes: []const u8) !*Self {
            var reader = io.Reader.fixed(bytes);
            const count = try reader.takeLeb128(u32);

            var self = Self{
                .bytes = bytes,
                .count = count,
            };
            return &self;
        }

        pub fn iter(self: *Self) !Iterator {
            var reader = io.Reader.fixed(self.bytes);
            _ = try reader.takeLeb128(u32);

            return Iterator{ .reader = reader, .count = self.count };
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
        };
    };
}

pub fn StreamableSection(comptime Stream: type) type {
    return struct {
        bytes: []const u8,
        count: u32,

        const Self = @This();

        pub fn fromBytes(bytes: []const u8) !*Self {
            var reader = io.Reader.fixed(bytes);
            const count = try reader.takeLeb128(u32);
            var self = Self{
                .bytes = bytes,
                .count = count,
            };
            return &self;
        }

        pub fn stream(self: *Self) !Stream {
            var reader = io.Reader.fixed(self.bytes);
            _ = try reader.takeLeb128(u32);
            return Stream{
                .reader = reader,
                .count = self.count,
            };
        }
    };
}
