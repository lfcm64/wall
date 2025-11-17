const std = @import("std");
const sections = @import("sections/sections.zig");
const types = @import("../module/types.zig");

const io = std.io;

const module = types.module;

pub const Parsed = union(enum) {
    CustomSection: module.CustomSection,

    TypeSection: *sections.TypeSection,
    ImportSection: *sections.ImportSection,
    FunctionSection: *sections.FunctionSection,
    TableSection: *sections.TableSection,
    MemorySection: *sections.MemorySection,
    GlobalSection: *sections.GlobalSection,
    ExportSection: *sections.ExportSection,

    StartSection: module.StartSection,

    ElementSection: *sections.ElementSection,
    CodeSection: *sections.CodeSection,
    DataSection: *sections.DataSection,

    ModuleHeader: module.ModuleHeader,
};

pub const Parser = struct {
    source: []const u8,
    reader: io.Reader,

    header_parsed: bool = false,
    last_section: ?module.SectionType = null,

    pub fn init(source: []const u8) Parser {
        return .{
            .source = source,
            .reader = io.Reader.fixed(source),
        };
    }

    pub fn reset(parser: *Parser) void {
        parser.reader.seek = 0;
        parser.header_parsed = false;
        parser.last_section = null;
    }

    pub fn parseNext(parser: *Parser) !?Parsed {
        if (parser.reader.seek == parser.reader.end) return null;

        if (!parser.header_parsed) {
            parser.header_parsed = true;
            return Parsed{
                .ModuleHeader = try types.module.ModuleHeader.fromReader(&parser.reader),
            };
        }
        const header = try module.SectionHeader.fromReader(&parser.reader);

        switch (header.type) {
            .Custom => return Parsed{
                .CustomSection = try module.CustomSection.fromReader(&parser.reader),
            },
            .Start => {
                parser.last_section = .Start;
                return Parsed{
                    .StartSection = try module.StartSection.fromReader(&parser.reader),
                };
            },
            inline else => |id| {
                const info = @typeInfo(Parsed);
                const field = info.@"union".fields[@intFromEnum(id)];
                const field_type = @typeInfo(field.type).pointer.child;

                const source = parser.source[parser.reader.seek..][0..header.size];
                try parser.reader.discardAll(header.size);

                parser.last_section = header.type;
                return @unionInit(
                    Parsed,
                    field.name,
                    try field_type.fromBytes(source),
                );
            },
        }
    }
};

test {
    const source = @embedFile("../tests/fib.wasm");

    var parser = Parser.init(source);
    _ = try parser.parseNext();

    const section = try parser.parseNext();

    const stream = section.?.TypeSection;

    while (try stream.next()) |streamed| {
        std.debug.print("{}\n", .{streamed});
    }
}
