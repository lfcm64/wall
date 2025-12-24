const Parser = @This();

const std = @import("std");
const wasm = @import("../wasm/wasm.zig");

const Event = @import("event.zig").Event;

const io = std.io;
const sections = wasm.sections;
const types = wasm.types;

const Section = sections.Section;

source: []const u8,
reader: io.Reader,

state: State = .not_started,

const log = std.log.scoped(.parser);

pub const State = union(enum) {
    not_started,
    header,
    section: sections.SectionType,
};

pub fn init(source: []const u8) Parser {
    return .{
        .source = source,
        .reader = io.Reader.fixed(source),
    };
}

pub fn reset(self: *Parser) void {
    self.reader.seek = 0;
    self.state = .not_started;
}

pub fn parseNext(self: *Parser) !?Event {
    if (self.reader.seek == self.source.len) return null;

    switch (self.state) {
        .not_started => {
            const header = try types.Header.fromReader(&self.reader);
            self.state = .header;
            return .{ .module_header = header };
        },
        .header, .section => {
            const section_type: sections.SectionType = @enumFromInt(try self.reader.takeByte());

            if (self.state == .section and section_type != .custom) {
                if (@intFromEnum(self.state.section) >= @intFromEnum(section_type)) {
                    return error.SectionsOutOfOrder;
                }
            }
            switch (section_type) {
                inline else => |ty| {
                    const info = @typeInfo(Event);
                    const field = info.@"union".fields[@intFromEnum(ty) + 1];

                    const section = try Section(ty).fromReader(&self.reader);
                    self.state = .{ .section = section_type };
                    return @unionInit(Event, field.name, section);
                },
            }
        },
    }
}
