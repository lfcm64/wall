const Parser = @This();

const std = @import("std");
const sections = @import("../core/sections.zig");

const io = std.io;

const Section = sections.Section;

bytes: []const u8,
reader: io.Reader,
state: State = .header,

pub const Payload = union(enum) {
    module_header: struct {
        magic: u32,
        version: u32,
    },
    custom_section: Section(.custom),
    type_section: Section(.type),
    import_section: Section(.import),
    function_section: Section(.func),
    table_section: Section(.table),
    memory_section: Section(.memory),
    global_section: Section(.global),
    export_section: Section(.@"export"),
    start_section: Section(.start),
    element_section: Section(.elem),
    code_section: Section(.code),
    data_section: Section(.data),
};

pub const State = enum {
    header,
    section,
};

pub fn init(bytes: []const u8) Parser {
    return .{
        .bytes = bytes,
        .reader = io.Reader.fixed(bytes),
    };
}

pub fn reset(self: *Parser) void {
    self.reader.seek = 0;
    self.state = .header;
}

pub fn parseNext(self: *Parser) !?Payload {
    const reader = &self.reader;
    if (reader.seek == self.reader.end) return null;

    if (self.state == .header) {
        self.state = .section;
        return Payload{
            .module_header = .{
                .magic = try reader.takeInt(u32, .little),
                .version = try reader.takeInt(u32, .little),
            },
        };
    }

    const section_type: sections.SectionType = @enumFromInt(try reader.takeByte());
    const size = try reader.takeLeb128(u32);

    const bytes = self.bytes[reader.seek..][0..size];
    try reader.discardAll(size);

    switch (section_type) {
        .custom => {
            const name_size = try reader.takeLeb128(u32);
            const name = try reader.take(name_size);

            const section_bytes_size = try reader.takeLeb128(u32);
            const section_bytes = try reader.take(section_bytes_size);

            return Payload{
                .custom_section = .{
                    .name = name,
                    .bytes = section_bytes,
                },
            };
        },
        .start => {
            const func_idx = try reader.takeLeb128(u32);
            return Payload{ .start_section = .{ .func_idx = func_idx } };
        },
        inline else => |ty| {
            const info = @typeInfo(Payload);
            const field = info.@"union".fields[@intFromEnum(ty) + 1];

            return @unionInit(
                Payload,
                field.name,
                try Section(ty).fromBytes(bytes),
            );
        },
    }
}
