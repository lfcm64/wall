const std = @import("std");

const io = std.io;

pub const VarU32 = struct {
    val: u32,

    pub fn fromReader(reader: *io.Reader) !VarU32 {
        return .{ .val = try reader.takeLeb128(u32) };
    }

    pub fn fromBytes(bytes: []const u8) !VarU32 {
        var reader = io.Reader.fixed(bytes);
        return fromReader(&reader);
    }
};

pub fn Vec(comptime Item: type) type {
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

        pub const Visitor = struct {
            ptr: *anyopaque,
            visit: *const fn (*anyopaque, Item, u32) anyerror!void,
        };

        pub fn visit(self: *const Self, visitor: Visitor) !void {
            var it = self.iter();
            var i: u32 = 0;
            while (try it.next()) |item| {
                try visitor.visit(visitor.ptr, item, i);
                i += 1;
            }
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
