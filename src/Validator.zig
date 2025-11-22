const Validator = @This();

const std = @import("std");
const types = @import("module/types.zig");
const module = @import("module/module.zig");

const Parser = @import("Parser.zig");
const Store = @import("Store.zig");

const Allocator = std.mem.Allocator;
const Section = module.Section;
const Payload = Parser.Payload;

allocator: Allocator,

store: Store = .{},
state: State = .not_started,

pub const State = enum(u8) {
    not_started,
    header,
    type,
    import,
    function,
    table,
    memory,
    global,
    @"export",
    start,
    element,
    code,
    data,
};

pub fn init(allocator: Allocator) Validator {
    return .{ .allocator = allocator };
}

pub fn deinit(self: *Validator) void {
    self.store.deinit(self.allocator);
}

pub fn validate(self: *Validator, payload: Payload) !void {
    switch (payload) {
        .module_header => |header| try self.validateModuleHeader(header),
        .type_section => |section| try self.validateTypeSection(section),
        .import_section => |section| try self.validateImportSection(section),
        .function_section => |section| try self.validateFuncSection(section),
        .table_section => |section| try self.validateTableSection(section),
        .memory_section => |section| try self.validateMemorySection(section),
        .global_section => |section| try self.validateGlobalSection(section),
        .export_section => |section| try self.validateExportSection(section),
        .start_section => |section| try self.validateStartSection(section),
        .element_section => |section| try self.validateElemSection(section),
        .code_section => |_| {},
        else => {},
    }
}

pub fn validateModuleHeader(self: *Validator, header: types.module.ModuleHeader) !void {
    if (self.state != .not_started) return error.UnexpectedHeader;
    self.state = .header;

    if (header.magic != 1836278016) return error.BadMagicNumber;
    if (header.version != 1) return error.BadVersionNumber;
}

pub fn validateTypeSection(self: *Validator, section: Section(.type)) !void {
    try self.checkSectionOrder(.type);
    self.state = .type;

    var it = section.iter();
    while (try it.next()) |func_type| {
        try self.store.append(.func_types, self.allocator, func_type);
    }
    if (!it.isFinished()) return error.RemainingBytesInSection;
}

pub fn validateImportSection(self: *Validator, section: Section(.import)) !void {
    try self.checkSectionOrder(.import);
    self.state = .import;

    var it = section.iter();
    while (try it.next()) |import| {
        if (!std.unicode.utf8ValidateSlice(import.module)) return error.InvalidImportModule;
        if (!std.unicode.utf8ValidateSlice(import.name)) return error.InvalidImportName;

        switch (import.kind) {
            .func => |func| try self.store.append(.func, self.allocator, func),
            .table => |table| try self.store.append(.tables, self.allocator, table),
            .memory => |memory| try self.store.append(.memories, self.allocator, memory),
            .global => |global| try self.store.append(.globals, self.allocator, global),
        }
    }
    if (!it.isFinished()) return error.RemainingBytesInSection;
}

pub fn validateFuncSection(self: *Validator, section: Section(.function)) !void {
    try self.checkSectionOrder(.function);
    self.state = .function;

    const type_count = self.store.count(.func_types);
    var it = section.iter();

    while (try it.next()) |func_index| {
        if (func_index.val >= type_count) return error.FunctionIndexOutOfBounds;
        try self.store.append(.func, self.allocator, func_index.val);
    }
    if (!it.isFinished()) return error.RemainingBytesInSection;
}

pub fn validateTableSection(self: *Validator, section: Section(.table)) !void {
    try self.checkSectionOrder(.table);
    self.state = .table;

    var it = section.iter();
    while (try it.next()) |table| {
        try self.store.append(.tables, self.allocator, table);
    }
    if (!it.isFinished()) return error.RemainingBytesInSection;
}

pub fn validateMemorySection(self: *Validator, section: Section(.memory)) !void {
    try self.checkSectionOrder(.memory);
    self.state = .memory;

    var it = section.iter();
    while (try it.next()) |memory| {
        try self.store.append(.memories, self.allocator, memory);
    }
    if (!it.isFinished()) return error.RemainingBytesInSection;
}

pub fn validateGlobalSection(self: *Validator, section: Section(.global)) !void {
    try self.checkSectionOrder(.global);
    self.state = .global;

    var it = section.iter();
    while (try it.next()) |global| {
        try self.store.append(.globals, self.allocator, global.type);
    }
    if (!it.isFinished()) return error.RemainingBytesInSection;
}

pub fn validateExportSection(self: *Validator, section: Section(.@"export")) !void {
    try self.checkSectionOrder(.@"export");
    self.state = .@"export";

    var export_map = std.StringHashMap(void).init(self.allocator);
    defer export_map.deinit();

    var it = section.iter();
    while (try it.next()) |exp| {
        if (!std.unicode.utf8ValidateSlice(exp.name)) return error.InvalidExportName;
        try export_map.put(exp.name, {});

        try self.store.append(.exports, self.allocator, exp);
    }
    if (!it.isFinished()) return error.RemainingBytesInSection;
}

pub fn validateStartSection(self: *Validator, section: Section(.start)) !void {
    try self.checkSectionOrder(.start);
    self.state = .start;

    if (section.val >= self.store.count(.func_types)) return error.StartSectionIndexOutOfBounds;
}

pub fn validateElemSection(self: *Validator, section: Section(.element)) !void {
    try self.checkSectionOrder(.element);
    self.state = .element;

    var it = section.iter();
    while (try it.next()) |elem| {
        if (elem.table_idx >= self.store.count(.tables))
            return error.InvalidElementTableIndex;
    }
    if (!it.isFinished()) return error.RemainingBytesInSection;
}

fn checkSectionOrder(self: *Validator, section_type: types.module.SectionType) !void {
    if (self.state == .not_started) return error.UnexpectedSection;
    if (@intFromEnum(self.state) == @intFromEnum(section_type) + 1) return error.DuplicateSection;
    if (@intFromEnum(self.state) > @intFromEnum(section_type) + 1) return error.UnexpectedSection;
}

