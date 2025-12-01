const std = @import("std");
const types = @import("types.zig");
const indices = @import("indices.zig");

const io = std.io;

pub const SectionType = enum(u8) {
    custom = 0x00,
    type = 0x01,
    import = 0x02,
    func = 0x03,
    table = 0x04,
    memory = 0x05,
    global = 0x06,
    @"export" = 0x07,
    start = 0x08,
    elem = 0x09,
    code = 0x0a,
    data = 0x0b,
};

pub fn Section(comptime section_type: SectionType) type {
    return switch (section_type) {
        .custom => CustomSection,
        .type => SectionLimited(types.FuncType, types.FuncType.fromReader),
        .import => SectionLimited(types.Import, types.Import.fromReader),
        .func => SectionLimited(indices.TypeIdx, types.VarU32.fromReader),
        .table => SectionLimited(types.Table, types.Table.fromReader),
        .memory => SectionLimited(types.Memory, types.Memory.fromReader),
        .global => SectionLimited(types.Global, types.Global.fromReader),
        .@"export" => SectionLimited(types.Export, types.Export.fromReader),
        .start => StartSection,
        .elem => SectionLimited(types.Element, types.Element.fromReader),
        .code => SectionLimited(types.FuncBody, types.FuncBody.fromReader),
        .data => SectionLimited(types.Segment, types.Segment.fromReader),
    };
}

pub fn SectionLimited(comptime T: type, read: *const fn (*io.Reader) anyerror!T) type {
    return struct {
        count: u32,
        bytes: []const u8,

        const Self = @This();

        pub fn fromBytes(bytes: []const u8) !Self {
            var reader = io.Reader.fixed(bytes);
            const count = try reader.takeLeb128(u32);

            return .{
                .count = count,
                .bytes = bytes[reader.seek..],
            };
        }

        pub fn visit(self: *const Self, visitor: Visitor) !void {
            var it = self.iter();
            var i: u32 = 0;

            while (try it.next()) |item| {
                try visitor.visit(visitor.ptr, item, i);
                i += 1;
            }
        }

        pub const Visitor = struct {
            ptr: *anyopaque,
            visit: *const fn (*anyopaque, T, u32) anyerror!void,
        };

        pub fn collect(self: *const Self, allocator: std.mem.Allocator) ![]T {
            const items = try allocator.alloc(T, self.count);
            errdefer allocator.free(items);

            var it = self.iter();
            var i: u32 = 0;
            while (try it.next()) |item| {
                items[i] = item;
                i += 1;
            }
            return items;
        }

        pub fn iter(self: *const Self) Iterator {
            return Iterator{
                .reader = io.Reader.fixed(self.bytes),
                .count = self.count,
            };
        }

        const Iterator = struct {
            reader: io.Reader,
            count: u32,
            index: u32 = 0,

            pub fn next(it: *Iterator) !?T {
                if (it.index == it.count) return null;
                const item = try read(&it.reader);
                it.index += 1;
                return item;
            }
        };
    };
}

pub const CustomSection = struct {
    name: []const u8,
    bytes: []const u8,

    pub fn fromBytes(bytes: []const u8) !CustomSection {
        var reader = io.Reader.fixed(bytes);

        const name_size = try reader.takeLeb128(u32);
        const name = try reader.take(name_size);

        const bytes_size = try reader.takeLeb128(u32);
        return .{
            .name = name,
            .bytes = try reader.take(bytes_size),
        };
    }
};

pub const StartSection = struct {
    func_idx: u32,

    pub fn fromBytes(bytes: []const u8) !StartSection {
        var reader = io.Reader.fixed(bytes);
        return .{ .func_idx = try reader.takeLeb128(u32) };
    }
};
