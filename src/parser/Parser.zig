const Parser = @This();

const std = @import("std");
const sections = @import("../core/sections.zig");

const Validator = @import("Validator.zig");

const io = std.io;
const Allocator = std.mem.Allocator;

const Section = sections.Section;

const log = std.log.scoped(.parser);

bytes: []const u8,

reader: io.Reader,
validator: Validator,

state: State = .not_started,

pub const ParsingEvent = union(enum) {
    ModuleHeader: struct {
        magic: u32,
        version: u32,
    },
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

pub const State = union(enum) {
    not_started,
    header,
    section: sections.SectionType,
    finished,
};

pub fn init(allocator: Allocator, bytes: []const u8) Parser {
    return .{
        .bytes = bytes,
        .reader = io.Reader.fixed(bytes),
        .validator = Validator.init(allocator),
    };
}

pub fn deinit(self: *Parser) void {
    self.validator.deinit();
}

pub fn reset(self: *Parser) void {
    self.reader.seek = 0;
    self.state = .header;
}

pub fn parseNext(self: *Parser) !?ParsingEvent {
    if (self.reader.seek == self.bytes.len) self.state = .finished;

    switch (self.state) {
        .not_started => {
            const magic = try self.reader.takeInt(u32, .little);
            const version = try self.reader.takeInt(u32, .little);

            log.info("module header parsed", .{});
            self.state = .header;

            return .{ .ModuleHeader = .{
                .magic = magic,
                .version = version,
            } };
        },
        .header, .section => {
            const section_type: sections.SectionType = @enumFromInt(try self.reader.takeByte());
            const section_size = try self.reader.takeLeb128(u32);

            const bytes = try self.reader.take(section_size);
            self.state = .{ .section = section_type };

            log.info("{} section parsed", .{section_type});
            switch (section_type) {
                inline else => |ty| {
                    const info = @typeInfo(ParsingEvent);
                    const field = info.@"union".fields[@intFromEnum(ty) + 1];

                    const section = try Section(ty).fromBytes(bytes);
                    if (ty != .custom) {
                        const func = @field(Validator, "validate" ++ field.name);
                        try func(&self.validator, section);
                    }
                    return @unionInit(
                        ParsingEvent,
                        field.name,
                        section,
                    );
                },
            }
        },
        .finished => return null,
    }
}

fn parseNextSection(self: *Parser) !?ParsingEvent {
    std.debug.assert(self.state == .header or self.state == .section);

    const section_type: sections.SectionType = @enumFromInt(try self.reader.takeByte());
    const section_size = try self.reader.takeLeb128(u32);

    const bytes = try self.reader.take(section_size);
    self.state = .{ .section = section_type };

    log.info("{} section parsed", .{section_type});

    switch (section_type) {
        .custom => return .{ .CustomSection = {} },
        .start => {
            const func_idx = try self.reader.takeLeb128(u32);
            const section = Section(.start){ .func_idx = func_idx };

            try self.validator.validateStartSection(section);
            return .{ .StartSection = section };
        },
        inline else => |ty| {
            const info = @typeInfo(ParsingEvent);
            const field = info.@"union".fields[@intFromEnum(ty) + 1];

            const section = try Section(ty).fromBytes(bytes);

            const func = @field(Validator, "validate" ++ field.name);
            try func(&self.validator, section);

            return @unionInit(
                ParsingEvent,
                field.name,
                section,
            );
        },
    }
}
