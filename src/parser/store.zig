const std = @import("std");
const types = @import("../module/types.zig");

const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

pub const ResourceType = enum {
    types,
    functions,
    tables,
    memories,
    globals,
    exports,
};

pub fn Resource(comptime resource_type: ResourceType) type {
    return switch (resource_type) {
        .types => types.function.FuncType,
        .functions => u32,
        .tables => types.table.Table,
        .memories => types.Memory,
        .globals => types.global.GlobalType,
        .exports => types.@"export".Export,
    };
}

pub const Store = struct {
    allocator: Allocator,

    types: ArrayList(types.function.FuncType) = .{},
    functions: ArrayList(u32) = .{},
    tables: ArrayList(types.table.Table) = .{},
    memories: ArrayList(types.Memory) = .{},
    globals: ArrayList(types.global.GlobalType) = .{},
    exports: ArrayList(types.@"export".Export) = .{},

    pub fn init(allocator: Allocator) Store {
        return .{ .allocator = allocator };
    }

    pub fn deinit(self: *Store, allocator: Allocator) void {
        self.types.deinit(allocator);
        self.functions.deinit(allocator);
        self.tables.deinit(allocator);
        self.memories.deinit(allocator);
        self.globals.deinit(allocator);
        self.exports.deinit(allocator);
    }

    pub fn append(self: *Store, comptime resource_type: ResourceType, resource: Resource(resource_type)) !void {
        const list = switch (resource_type) {
            .types => &self.types,
            .functions => &self.functions,
            .tables => &self.tables,
            .memories => &self.memories,
            .globals => &self.globals,
            .exports => &self.exports,
        };
        try list.append(self.allocator, resource);
    }

    pub fn get(self: *Store, comptime resource_type: ResourceType, n: u32) ?Resource(resource_type) {
        const list = switch (resource_type) {
            .types => &self.types,
            .functions => &self.functions,
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
            .types => &self.types,
            .functions => &self.functions,
            .tables => &self.tables,
            .memories => &self.memories,
            .globals => &self.globals,
            .exports => &self.exports,
        };
        return @intCast(list.items.len);
    }
};
