const Context = @This();

const std = @import("std");
const wasm = @import("../wasm/wasm.zig");

const Payload = @import("event.zig").Payload;

const types = wasm.types;
const sections = wasm.sections;
const indices = wasm.indices;

const Allocator = std.mem.Allocator;
const Section = sections.Section;

allocator: std.mem.Allocator,

functypes: []types.FuncType = &[_]types.FuncType{},
imports: Imports = .{},

funcs: []indices.Func = &[_]indices.Func{},
tables: []types.Table = &[_]types.Table{},
memories: []types.Memory = &[_]types.Memory{},
globals: []types.Global = &[_]types.Global{},

exports: []types.Export = &[_]types.Export{},
start: ?u32 = null,

pub const Imports = struct {
    funcs: []indices.Func = &[_]indices.Func{},
    tables: []types.Table = &[_]types.Table{},
    memories: []types.Memory = &[_]types.Memory{},
    globals: []types.GlobalType = &[_]types.GlobalType{},
};

pub fn init(allocator: std.mem.Allocator) Context {
    return .{ .allocator = allocator };
}

pub fn deinit(self: *Context) void {
    self.allocator.free(self.functypes);
    self.allocator.free(self.imports.funcs);
    self.allocator.free(self.imports.tables);
    self.allocator.free(self.imports.memories);
    self.allocator.free(self.imports.globals);
    self.allocator.free(self.funcs);
    self.allocator.free(self.tables);
    self.allocator.free(self.memories);
    self.allocator.free(self.globals);
    self.allocator.free(self.exports);
}

pub fn receivePayload(self: *Context, payload: Payload) !void {
    switch (payload) {
        .type_section => |section| self.functypes = try section.collect(self.allocator),
        .import_section => |section| try self.receiveImportSection(section),
        .func_section => |section| self.funcs = try section.collect(self.allocator),
        .table_section => |section| self.tables = try section.collect(self.allocator),
        .memory_section => |section| self.memories = try section.collect(self.allocator),
        .global_section => |section| self.globals = try section.collect(self.allocator),
        .export_section => |section| self.exports = try section.collect(self.allocator),
        .start_section => |section| self.start = section.func_idx,
        else => {},
    }
}

fn receiveImportSection(self: *Context, section: sections.Section(.import)) !void {
    var funcs: std.ArrayList(indices.Func) = .{};
    var tables: std.ArrayList(types.Table) = .{};
    var memories: std.ArrayList(types.Memory) = .{};
    var globals: std.ArrayList(types.GlobalType) = .{};

    var it = section.iter();
    while (try it.next()) |import| {
        switch (import.desc) {
            .func => |type_idx| try funcs.append(self.allocator, type_idx),
            .table => |table| try tables.append(self.allocator, table),
            .memory => |mem| try memories.append(self.allocator, mem),
            .global => |global_type| try globals.append(self.allocator, global_type),
        }
    }

    self.imports.funcs = try funcs.toOwnedSlice(self.allocator);
    self.imports.tables = try tables.toOwnedSlice(self.allocator);
    self.imports.memories = try memories.toOwnedSlice(self.allocator);
    self.imports.globals = try globals.toOwnedSlice(self.allocator);
}

pub fn typeOfFunc(self: *const Context, idx: u32) types.FuncType {
    const type_idx = if (idx < self.imports.funcs.len)
        self.imports.funcs[idx]
    else
        self.funcs[idx - self.imports.funcs.len];
    return self.functypes[type_idx];
}

pub fn typeOfGlobal(self: *const Context, idx: u32) types.GlobalType {
    if (idx < self.imports.globals.len) {
        return self.imports.globals[idx];
    }
    return self.globals[idx - self.imports.globals.len].ty;
}

pub fn funcCount(self: *const Context) u32 {
    return @intCast(self.imports.funcs.len + self.funcs.len);
}

pub fn globalCount(self: *const Context) u32 {
    return @intCast(self.imports.globals.len + self.globals.len);
}

pub fn tableCount(self: *const Context) u32 {
    return @intCast(self.imports.tables.len + self.tables.len);
}

pub fn memoryCount(self: *const Context) u32 {
    return @intCast(self.imports.memories.len + self.memories.len);
}
