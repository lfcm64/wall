const std = @import("std");
const types = @import("../module/types.zig");

const io = std.io;

const module = types.module;
const sections = types.sections;

pub const Payload = union(enum) {
    custom_section: sections.CustomSection,
    type_section: sections.TypeSection,
    import_section: sections.ImportSection,
    function_section: sections.FunctionSection,
    table_section: sections.TableSection,
    memory_section: sections.MemorySection,
    global_section: sections.GlobalSection,
    export_section: sections.ExportSection,
    start_section: sections.StartSection,
    element_section: sections.ElementSection,
    code_section: sections.CodeSection,
    data_section: sections.DataSection,

    module_header: module.ModuleHeader,
};

pub const Parser = struct {
    bytes: []const u8,
    reader: io.Reader,

    header_parsed: bool = false,

    pub fn init(bytes: []const u8) Parser {
        return .{
            .bytes = bytes,
            .reader = io.Reader.fixed(bytes),
        };
    }

    pub fn reset(parser: *Parser) void {
        parser.reader.seek = 0;
        parser.header_parsed = false;
    }

    pub fn parseNext(parser: *Parser) !?Payload {
        if (parser.reader.seek == parser.reader.end) return null;

        if (!parser.header_parsed) {
            parser.header_parsed = true;
            return Payload{
                .module_header = try module.ModuleHeader.fromReader(&parser.reader),
            };
        }
        const header = try sections.SectionHeader.fromReader(&parser.reader);
        switch (header.type) {
            .Custom => return Payload{
                .custom_section = try sections.CustomSection.fromReader(&parser.reader),
            },
            .Start => return Payload{
                .start_section = try sections.StartSection.fromReader(&parser.reader),
            },
            inline else => |id| {
                const info = @typeInfo(Payload);
                const field = info.@"union".fields[@intFromEnum(id)];

                const bytes = parser.bytes[parser.reader.seek..][0..header.size];
                try parser.reader.discardAll(header.size);

                return @unionInit(
                    Payload,
                    field.name,
                    try field.type.fromBytes(bytes),
                );
            },
        }
    }
};
