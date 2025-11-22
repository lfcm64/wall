const Store = @This();

const std = @import("std");
const types = @import("module/types.zig");

const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

func_types: ArrayList(types.Function.Type) = .{},
func: ArrayList(u32) = .{},
tables: ArrayList(types.Table) = .{},
memories: ArrayList(types.Memory) = .{},
globals: ArrayList(types.Global.Type) = .{},
exports: ArrayList(types.Export) = .{},

pub const ResourceType = enum {
    func_types,
    func,
    tables,
    memories,
    globals,
    exports,
};

pub fn Resource(comptime resource_type: ResourceType) type {
    return switch (resource_type) {
        .func_types => types.Function.Type,
        .func => u32,
        .tables => types.Table,
        .memories => types.Memory,
        .globals => types.Global.Type,
        .exports => types.Export,
    };
}

pub fn deinit(self: *Store, allocator: Allocator) void {
    self.func_types.deinit(allocator);
    self.func.deinit(allocator);
    self.tables.deinit(allocator);
    self.memories.deinit(allocator);
    self.globals.deinit(allocator);
    self.exports.deinit(allocator);
}

pub fn append(
    self: *Store,
    comptime resource_type: ResourceType,
    allocator: Allocator,
    resource: Resource(resource_type),
) !void {
    const list = switch (resource_type) {
        .func_types => &self.func_types,
        .func => &self.func,
        .tables => &self.tables,
        .memories => &self.memories,
        .globals => &self.globals,
        .exports => &self.exports,
    };
    try list.append(allocator, resource);
}

pub fn get(self: *Store, comptime resource_type: ResourceType, n: u32) ?Resource(resource_type) {
    const list = switch (resource_type) {
        .func_types => &self.func_types,
        .func => &self.func,
        .tables => &self.tables,
        .memories => &self.memories,
        .globals => &self.globals,
        .exports => &self.exports,
    };
    if (n >= list.items.len) return null;
    return list.items[n];
}

pub fn count(self: *Store, comptime resource_type: ResourceType) u32 {
    const list = switch (resource_type) {
        .func_types => &self.func_types,
        .func => &self.func,
        .tables => &self.tables,
        .memories => &self.memories,
        .globals => &self.globals,
        .exports => &self.exports,
    };
    return @intCast(list.items.len);
}
