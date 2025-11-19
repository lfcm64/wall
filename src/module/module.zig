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
