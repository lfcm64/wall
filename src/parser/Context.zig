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

pub const IndexError = error{
    FuncTypeIndexOutOfBounds,
    FuncIndexOutOfBounds,
    TableIndexOutOfBounds,
    MemoryIndexOutOfBounds,
    GlobalIndexOutOfBounds,
    ExportNotFound,
};

pub fn deinit(self: *Context, allocator: Allocator) void {
    self.functypes.deinit(allocator);
    self.funcs.deinit(allocator);
    self.tables.deinit(allocator);
    self.memories.deinit(allocator);
    self.globals.deinit(allocator);
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
    switch (import.desc) {
        .func => |func| try self.addFunc(allocator, func),
        .global => |global| try self.globals.append(allocator, global),
        .memory => |mem| try self.addMemory(allocator, mem),
        .table => |table| try self.addTable(allocator, table),
    }
}

// Getter functions
pub fn getFuncType(self: *const Context, idx: u32) IndexError!types.FuncType {
    if (idx >= self.functypes.items.len) {
        return IndexError.FuncTypeIndexOutOfBounds;
    }
    return self.functypes.items[idx];
}

pub fn getFunc(self: *const Context, idx: u32) IndexError!indices.FuncIdx {
    if (idx >= self.funcs.items.len) {
        return IndexError.FuncIndexOutOfBounds;
    }
    return self.funcs.items[idx];
}

pub fn getTable(self: *const Context, idx: u32) IndexError!types.Table {
    if (idx >= self.tables.items.len) {
        return IndexError.TableIndexOutOfBounds;
    }
    return self.tables.items[idx];
}

pub fn getMemory(self: *const Context, idx: u32) IndexError!types.Memory {
    if (idx >= self.memories.items.len) {
        return IndexError.MemoryIndexOutOfBounds;
    }
    return self.memories.items[idx];
}

pub fn getGlobal(self: *const Context, idx: u32) IndexError!types.GlobalType {
    if (idx >= self.globals.items.len) {
        return IndexError.GlobalIndexOutOfBounds;
    }
    return self.globals.items[idx];
}

pub fn getExport(self: *const Context, name: []const u8) IndexError!types.Export {
    return self.exports.get(name) orelse IndexError.ExportNotFound;
}

pub fn typeOfFunc(self: *const Context, idx: u32) IndexError!types.FuncType {
    const func_type_idx = try self.getFunc(idx);
    return try self.getFuncType(func_type_idx);
}

// Count functions
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
