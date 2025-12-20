const Module = @This();

const std = @import("std");
const wasm = @import("wasm/wasm.zig");
const validation = @import("validation/validation.zig");

const Parser = @import("parser/Parser.zig");
const Handler = @import("parser/Handler.zig");

const types = wasm.types;
const sections = wasm.sections;

const Allocator = std.mem.Allocator;
const Section = sections.Section;

allocator: std.mem.Allocator,

magic: u32 = 0,
version: u32 = 0,

functypes: std.ArrayList(types.FuncType) = .{},
imports: std.ArrayList(types.Import) = .{},
funcs: std.ArrayList(u32) = .{},
tables: std.ArrayList(types.Table) = .{},
memories: std.ArrayList(types.Memory) = .{},
globals: std.ArrayList(types.Global) = .{},
exports: std.ArrayList(types.Export) = .{},
start: ?u32 = null,
elems: std.ArrayList(types.Element) = .{},
code: std.ArrayList(types.FuncBody) = .{},
data: std.ArrayList(types.Segment) = .{},

custom_section_num: u32 = 0,

pub fn init(allocator: std.mem.Allocator) Module {
    return .{ .allocator = allocator };
}

pub fn deinit(self: *Module) void {
    self.functypes.deinit(self.allocator);
    self.imports.deinit(self.allocator);
    self.funcs.deinit(self.allocator);
    self.tables.deinit(self.allocator);
    self.memories.deinit(self.allocator);
    self.globals.deinit(self.allocator);
    self.exports.deinit(self.allocator);
    self.elems.deinit(self.allocator);
    self.code.deinit(self.allocator);
    self.data.deinit(self.allocator);
}

pub fn parse(self: *Module, parser: *Parser) !void {
    try parser.parseAll(self.handler());
}

pub fn validate(self: *Module) !void {
    try validation.validateModule(self.allocator, self);
}

pub fn typeOfFunc(self: *const Module, idx: u32) types.FuncType {
    const type_idx = self.funcs.items[idx];
    return self.functypes.items[type_idx];
}

pub fn handler(self: *Module) Handler {
    return .{
        .ptr = self,
        .vtable = &.{
            .onModuleHeader = onModuleHeader,
            .onCustomSection = onCustomSection,
            .onTypeSection = onTypeSection,
            .onImportSection = onImportSection,
            .onFuncSection = onFuncSection,
            .onTableSection = onTableSection,
            .onMemorySection = onMemorySection,
            .onGlobalSection = onGlobalSection,
            .onExportSection = onExportSection,
            .onStartSection = onStartSection,
            .onElementSection = onElementSection,
            .onCodeSection = onCodeSection,
            .onDataSection = onDataSection,
        },
    };
}

fn onModuleHeader(ptr: *anyopaque, header: types.Header) !void {
    const self: *Module = @ptrCast(@alignCast(ptr));
    self.magic = header.magic;
    self.version = header.version;
}

fn onCustomSection(ptr: *anyopaque, _: Section(.custom)) !void {
    const self: *Module = @ptrCast(@alignCast(ptr));
    self.custom_section_num += 1;
}

fn onTypeSection(ptr: *anyopaque, section: Section(.type)) !void {
    const self: *Module = @ptrCast(@alignCast(ptr));
    var it = section.iter();
    while (try it.next()) |func_type| {
        try self.functypes.append(self.allocator, func_type);
    }
}

fn onImportSection(ptr: *anyopaque, section: Section(.import)) !void {
    const self: *Module = @ptrCast(@alignCast(ptr));
    var it = section.iter();
    while (try it.next()) |import| {
        try self.imports.append(self.allocator, import);
    }
}

fn onFuncSection(ptr: *anyopaque, section: Section(.func)) !void {
    const self: *Module = @ptrCast(@alignCast(ptr));
    var it = section.iter();
    while (try it.next()) |type_idx| {
        try self.funcs.append(self.allocator, type_idx);
    }
}

fn onTableSection(ptr: *anyopaque, section: Section(.table)) !void {
    const self: *Module = @ptrCast(@alignCast(ptr));
    var it = section.iter();
    while (try it.next()) |table| {
        try self.tables.append(self.allocator, table);
    }
}

fn onMemorySection(ptr: *anyopaque, section: Section(.memory)) !void {
    const self: *Module = @ptrCast(@alignCast(ptr));
    var it = section.iter();
    while (try it.next()) |memory| {
        try self.memories.append(self.allocator, memory);
    }
}

fn onGlobalSection(ptr: *anyopaque, section: Section(.global)) !void {
    const self: *Module = @ptrCast(@alignCast(ptr));
    var it = section.iter();
    while (try it.next()) |global| {
        try self.globals.append(self.allocator, global);
    }
}

fn onExportSection(ptr: *anyopaque, section: Section(.@"export")) !void {
    const self: *Module = @ptrCast(@alignCast(ptr));
    var it = section.iter();
    while (try it.next()) |exp| {
        try self.exports.append(self.allocator, exp);
    }
}

fn onStartSection(ptr: *anyopaque, section: Section(.start)) !void {
    const self: *Module = @ptrCast(@alignCast(ptr));
    self.start = section.func_idx;
}

fn onElementSection(ptr: *anyopaque, section: Section(.elem)) !void {
    const self: *Module = @ptrCast(@alignCast(ptr));
    var it = section.iter();
    while (try it.next()) |element| {
        try self.elems.append(self.allocator, element);
    }
}

fn onCodeSection(ptr: *anyopaque, section: Section(.code)) !void {
    const self: *Module = @ptrCast(@alignCast(ptr));
    var it = section.iter();
    while (try it.next()) |code| {
        try self.code.append(self.allocator, code);
    }
}

fn onDataSection(ptr: *anyopaque, section: Section(.data)) !void {
    const self: *Module = @ptrCast(@alignCast(ptr));
    var it = section.iter();
    while (try it.next()) |segment| {
        try self.data.append(self.allocator, segment);
    }
}
