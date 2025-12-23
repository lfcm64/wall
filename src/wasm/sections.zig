const std = @import("std");

const types = @import("types.zig");
const indices = @import("indices.zig");
const SizedVec = @import("vecs.zig").SizedVec;

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
        .type => SizedVec(types.FuncType),
        .import => SizedVec(types.Import),
        .func => SizedVec(u32),
        .table => SizedVec(types.Table),
        .memory => SizedVec(types.Memory),
        .global => SizedVec(types.Global),
        .@"export" => SizedVec(types.Export),
        .start => StartSection,
        .elem => SizedVec(types.Element),
        .code => SizedVec(types.FuncBody),
        .data => SizedVec(types.Segment),
    };
}

const CustomSection = struct {
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

const StartSection = struct {
    func_idx: indices.Func,

    pub fn fromReader(reader: *io.Reader) !StartSection {
        return .{ .func_idx = try reader.takeLeb128(u32) };
    }
};

pub fn SectionItem(section_type: SectionType) type {
    return switch (section_type) {
        .type => types.FuncType,
        .import => types.Import,
        .func => u32,
        .table => types.Table,
        .memory => types.Memory,
        .global => types.Global,
        .@"export" => types.Export,
        .elem => types.Element,
        .code => types.FuncBody,
        .data => types.Segment,
        else => unreachable,
    };
}
