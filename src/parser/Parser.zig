const Parser = @This();

const std = @import("std");
const wasm = @import("../wasm/wasm.zig");
const event = @import("event.zig");

const Context = @import("Context.zig");

const io = std.io;
const sections = wasm.sections;
const types = wasm.types;

const Section = sections.Section;
const Payload = event.Payload;
const Event = event.Event;

source: []const u8,
reader: io.Reader,

ctx: *Context,

state: State = .header,

const log = std.log.scoped(.parser);

pub const State = enum {
    header,
    section,
};

pub fn init(source: []const u8, ctx: *Context) Parser {
    return .{
        .source = source,
        .reader = io.Reader.fixed(source),
        .ctx = ctx,
    };
}

pub fn reset(self: *Parser) void {
    self.reader.seek = 0;
    self.state = .header;
}

pub fn parseNext(self: *Parser) !?Event {
    if (self.reader.seek == self.source.len) return null;
    var payload: Payload = undefined;

    switch (self.state) {
        .header => {
            const header = try types.Header.fromReader(&self.reader);
            self.state = .section;
            payload = .{ .ModuleHeader = header };
        },
        .section => {
            const section_type: sections.SectionType = @enumFromInt(try self.reader.takeByte());
            switch (section_type) {
                inline else => |ty| {
                    const info = @typeInfo(Payload);
                    const field = info.@"union".fields[@intFromEnum(ty) + 1];

                    const section = try Section(ty).fromReader(&self.reader);
                    payload = @unionInit(Payload, field.name, section);
                },
            }
        },
    }
    try self.ctx.onPayload(payload);
    return Event{ .ctx = self.ctx, .payload = payload };
}
