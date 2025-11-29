const Parser = @This();

const std = @import("std");
const module = @import("../module/module.zig");

const io = std.io;

const ModuleHeader = module.ModuleHeader;
const SectionHeader = module.SectionHeader;
const Section = module.Section;

bytes: []const u8,
reader: io.Reader,
state: State = .header,

pub const Payload = union(enum) {
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

    module_header: struct {
        magic: u32,
        version: u32,
    },
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

    const header = try SectionHeader.fromReader(reader);

    const bytes = self.bytes[reader.seek..][0..header.size];
    try reader.discardAll(header.size);

    switch (header.type) {
        inline else => |section_type| {
            const info = @typeInfo(Payload);
            const field = info.@"union".fields[@intFromEnum(section_type)];

            return @unionInit(
                Payload,
                field.name,
                try Section(section_type).fromBytes(bytes),
            );
        },
    }
}
