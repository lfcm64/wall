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
        .custom => struct {
            name: []const u8,
            bytes: []const u8,
        },
        .type => SectionLimited(types.FuncType, types.FuncType.fromReader),
        .import => SectionLimited(types.Import, types.Import.fromReader),
        .func => SectionLimited(indices.TypeIdx, types.VarU32.fromReader),
        .table => SectionLimited(types.Table, types.Table.fromReader),
        .memory => SectionLimited(types.Memory, types.Memory.fromReader),
        .global => SectionLimited(types.Global, types.Global.fromReader),
        .@"export" => SectionLimited(types.Export, types.Export.fromReader),
        .start => indices.FuncIdx,
        .elem => SectionLimited(types.Element, types.Element.fromReader),
        .code => SectionLimited(types.FuncBody, types.FuncBody.fromReader),
        .data => SectionLimited(types.Segment, types.Element.fromReader),
    };
}

pub fn SectionLimited(comptime T: type, read: *const fn (*io.Reader) anyerror!T) type {
    return struct {
        bytes: []const u8,
        count: u32,

        const Self = @This();

        pub fn fromBytes(bytes: []const u8) !Self {
            var reader = io.Reader.fixed(bytes);
            const count = try reader.takeLeb128(u32);

            return .{
                .bytes = bytes[reader.seek..],
                .count = count,
            };
        }

        pub fn visit(self: *const Self, visitor: Visitor) !void {
            var it = self.iter();
            while (try it.next()) |item| {
                try visitor.visit(visitor.ptr, item);
            }
        }

        pub const Visitor = struct {
            ptr: *anyopaque,
            visit: *const fn (*anyopaque, T) anyerror!void,
        };

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
