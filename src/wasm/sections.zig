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
};

const StartSection = struct {
    func_idx: indices.Func,
};
