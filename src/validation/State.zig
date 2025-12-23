const State = @This();

const std = @import("std");
const wasm = @import("../wasm/wasm.zig");

const types = wasm.types;
const sections = wasm.sections;
const indices = wasm.indices;

const Allocator = std.mem.Allocator;
const Section = sections.Section;

allocator: std.mem.Allocator,

functypes: std.ArrayList(types.FuncType) = .{},
funcs: std.ArrayList(indices.Func) = .{},
tables: std.ArrayList(types.Table) = .{},
memories: std.ArrayList(types.Memory) = .{},
globals: std.ArrayList(types.GlobalType) = .{},

imported_funcs: u32 = 0,
imported_tables: u32 = 0,
imported_memories: u32 = 0,
imported_globals: u32 = 0,

imports: std.StringArrayHashMapUnmanaged(void) = .{},
exports: std.StringArrayHashMapUnmanaged(void) = .{},

pub fn init(allocator: std.mem.Allocator) State {
    return .{ .allocator = allocator };
}

pub fn deinit(self: *State) void {
    self.functypes.deinit(self.allocator);
    self.funcs.deinit(self.allocator);
    self.tables.deinit(self.allocator);
    self.memories.deinit(self.allocator);
    self.globals.deinit(self.allocator);

    self.imports.deinit(self.allocator);
    self.exports.deinit(self.allocator);
}

pub fn addFuncType(self: *State, func_type: types.FuncType) !void {
    try self.functypes.append(self.allocator, func_type);
}

pub fn addFunc(self: *State, type_idx: u32) !void {
    try self.funcs.append(self.allocator, type_idx);
}

pub fn addTable(self: *State, table: types.Table) !void {
    try self.tables.append(self.allocator, table);
}

pub fn addMemory(self: *State, memory: types.Memory) !void {
    try self.memories.append(self.allocator, memory);
}

pub fn addGlobal(self: *State, global: types.Global) !void {
    try self.globals.append(self.allocator, global.ty);
}

pub fn addExport(self: *State, exp: types.Export) !void {
    try self.exports.put(self.allocator, exp.name, {});
}

pub fn addImport(self: *State, import: types.Import) !void {
    const key = try std.fmt.allocPrint(
        self.allocator,
        "{s}.{s}",
        .{ import.module, import.name },
    );
    defer self.allocator.free(key);

    try self.imports.put(self.allocator, key, {});

    switch (import.desc) {
        .func => |type_idx| {
            self.imported_funcs += 1;
            try self.funcs.append(self.allocator, type_idx);
        },
        .table => |table| {
            self.imported_tables += 1;
            try self.tables.append(self.allocator, table);
        },
        .memory => |memory| {
            self.imported_memories += 1;
            try self.memories.append(self.allocator, memory);
        },
        .global => |global_type| {
            self.imported_globals += 1;
            try self.globals.append(self.allocator, global_type);
        },
    }
}

pub fn typeOfFunc(self: *State, idx: u32) types.FuncType {
    const type_idx = self.funcs.items[idx];
    return self.functypes.items[type_idx];
}
