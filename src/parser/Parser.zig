const Parser = @This();

const std = @import("std");
const wasm = @import("../wasm/wasm.zig");

const Handler = @import("Handler.zig");

const io = std.io;
const sections = wasm.sections;
const types = wasm.types;

const Section = sections.Section;

source: []const u8,
reader: io.Reader,
state: State = .header,

const log = std.log.scoped(.parser);

pub const State = enum {
    header,
    section,
};

pub const Event = union(enum) {
    ModuleHeader: types.Header,
    CustomSection: Section(.custom),
    TypeSection: Section(.type),
    ImportSection: Section(.import),
    FuncSection: Section(.func),
    TableSection: Section(.table),
    MemorySection: Section(.memory),
    GlobalSection: Section(.global),
    ExportSection: Section(.@"export"),
    StartSection: Section(.start),
    ElementSection: Section(.elem),
    CodeSection: Section(.code),
    DataSection: Section(.data),
};

pub fn init(source: []const u8) Parser {
    return .{
        .source = source,
        .reader = io.Reader.fixed(source),
    };
}

pub fn reset(self: *Parser) void {
    self.reader.seek = 0;
    self.state = .header;
}

pub fn parseAll(self: *Parser, handler: Handler) !void {
    while (try self.parseNext(handler)) {}
}

pub fn parseNext(self: *Parser, handler: Handler) !bool {
    if (self.reader.seek == self.source.len) return false;

    switch (self.state) {
        .header => {
            const magic = try self.reader.takeInt(u32, .little);
            const version = try self.reader.takeInt(u32, .little);
            self.state = .section;
            try handler.onEvent(.{ .ModuleHeader = .{
                .magic = magic,
                .version = version,
            } });
        },
        .section => try self.parseNextSection(handler),
    }
    return true;
}

fn parseNextSection(self: *Parser, handler: Handler) !void {
    const section_type: wasm.sections.SectionType = @enumFromInt(try self.reader.takeByte());
    var event: Event = undefined;
    switch (section_type) {
        .custom => {
            event = Event{ .CustomSection = undefined };
        },
        .start => {
            const func_idx = try self.reader.takeLeb128(u32);
            const section = Section(.start){ .func_idx = func_idx };
            event = Event{ .StartSection = section };
        },
        inline else => |ty| {
            const info = @typeInfo(Event);
            const field = info.@"union".fields[@intFromEnum(ty) + 1];

            const section = try Section(ty).fromReader(&self.reader);
            event = @unionInit(Event, field.name, section);
        },
    }
    try handler.onEvent(event);
}
