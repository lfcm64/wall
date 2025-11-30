const Context = @This();

const std = @import("std");
const types = @import("../core/types.zig");
const indices = @import("../core/indices.zig");

const Allocator = std.mem.Allocator;

functypes: std.ArrayList(types.FuncType) = .{},
funcs: std.ArrayList(indices.FuncIdx) = .{},
tables: std.ArrayList(types.Table) = .{},
memories: std.ArrayList(types.Memory) = .{},
globals: std.ArrayList(types.GlobalType) = .{},

exports: std.StringArrayHashMapUnmanaged(types.Export) = .{},

pub fn deinit(self: *Context, allocator: Allocator) void {
    self.functypes.deinit(allocator);
    self.funcs.deinit(allocator);
    self.tables.deinit(allocator);
    self.memories.deinit(allocator);
    self.exports.deinit(allocator);
}

pub fn addFuncType(self: *Context, allocator: Allocator, func_type: types.FuncType) !void {
    try self.functypes.append(allocator, func_type);
}

pub fn addFunc(self: *Context, allocator: Allocator, type_idx: indices.FuncIdx) !void {
    try self.funcs.append(allocator, type_idx);
}

pub fn addTable(self: *Context, allocator: Allocator, table: types.Table) !void {
    try self.tables.append(allocator, table);
}

pub fn addMemory(self: *Context, allocator: Allocator, memory: types.Memory) !void {
    try self.memories.append(allocator, memory);
}

pub fn addGlobal(self: *Context, allocator: Allocator, global: types.Global) !void {
    try self.globals.append(allocator, global.ty);
}

pub fn addExport(self: *Context, allocator: Allocator, exp: types.Export) !void {
    try self.exports.put(allocator, exp.name, exp);
}

pub fn addImport(self: *Context, allocator: Allocator, import: types.Import) !void {
    switch (import.kind) {
        .func => |func| try self.addFunc(allocator, func),
        .global => |global| try self.globals.append(allocator, global),
        .memory => |mem| try self.addMemory(allocator, mem),
        .table => |table| try self.addTable(allocator, table),
    }
}

pub fn getFuncTypeByFuncIdx(self: *const Context, idx: u32) types.FuncType {
    const func_type_idx = self.funcs.items[idx];
    return self.functypes.items[func_type_idx];
}

pub fn funcTypeCount(self: *const Context) u32 {
    return @intCast(self.functypes.items.len);
}

pub fn funcCount(self: *const Context) u32 {
    return @intCast(self.funcs.items.len);
}

pub fn tableCount(self: *const Context) u32 {
    return @intCast(self.tables.items.len);
}

pub fn memoryCount(self: *const Context) u32 {
    return @intCast(self.memories.items.len);
}

pub fn globalCount(self: *const Context) u32 {
    return @intCast(self.globals.items.len);
}
