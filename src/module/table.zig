const std = @import("std");
const types = @import("types.zig");

const io = std.io;
const assert = std.debug.assert;

const Limits = types.Limits;

pub const Table = struct {
    limits: Limits,

    pub fn fromReader(reader: *io.Reader) !Table {
        const tag = try reader.takeByte();
        assert(tag == 0x70);

        return .{ .limits = try Limits.fromReader(reader) };
    }
};
