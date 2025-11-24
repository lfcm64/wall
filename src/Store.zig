const Store = @This();

const std = @import("std");
const types = @import("module/types.zig");

const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const Vec = types.primitives.Vec;
const VarU32 = types.primitives.VarU32;

func_types: ArrayList(types.Function.Type) = .{},
funcs: ArrayList(u32) = .{},
tables: ArrayList(types.Table) = .{},
memories: ArrayList(types.Memory) = .{},
globals: ArrayList(types.Global.Type) = .{},

pub const ResourceType = enum {
    func_types,
    funcs,
    tables,
    memories,
    globals,
};

pub fn Resource(comptime resource_type: ResourceType) type {
    return switch (resource_type) {
        .func_types => types.Function.Type,
        .funcs => VarU32,
        .tables => types.Table,
        .memories => types.Memory,
        .globals => types.Global,
    };
}

pub fn deinit(self: *Store, allocator: Allocator) void {
    self.func_types.deinit(allocator);
    self.funcs.deinit(allocator);
    self.tables.deinit(allocator);
    self.memories.deinit(allocator);
    self.globals.deinit(allocator);
}

pub fn appendResources(
    self: *Store,
    comptime resource_type: ResourceType,
    allocator: Allocator,
    vec: Vec(Resource(resource_type)),
) !void {
    var it = vec.iter();
    while (try it.next()) |resource| {
        switch (resource_type) {
            .func_types => try self.func_types.append(allocator, resource),
            .funcs => try self.funcs.append(allocator, resource.val),
            .tables => try self.tables.append(allocator, resource),
            .memories => try self.memories.append(allocator, resource),
            .globals => try self.globals.append(allocator, resource.type),
        }
    }
    if (!it.isFinished()) return error.RemainingBytesInSection;
}

pub fn appendImports(
    self: *Store,
    allocator: Allocator,
    vec: Vec(types.Import),
) !void {
    var it = vec.iter();
    while (try it.next()) |import| {
        switch (import.kind) {
            .func => |func| try self.funcs.append(allocator, func),
            .table => |table| try self.tables.append(allocator, table),
            .memory => |memory| try self.memories.append(allocator, memory),
            .global => |global| try self.globals.append(allocator, global),
        }
    }
    if (!it.isFinished()) return error.RemainingBytesInSection;
}

pub fn get(self: *Store, comptime resource_type: ResourceType, n: u32) ?Resource(resource_type) {
    const list = switch (resource_type) {
        .func_types => &self.func_types,
        .funcs => &self.funcs,
        .tables => &self.tables,
        .memories => &self.memories,
        .globals => &self.globals,
    };
    if (n >= list.items.len) return null;
    return list.items[n];
}

pub fn count(self: *Store, comptime resource_type: ResourceType) u32 {
    const list = switch (resource_type) {
        .func_types => &self.func_types,
        .funcs => &self.funcs,
        .tables => &self.tables,
        .memories => &self.memories,
        .globals => &self.globals,
    };
    return @intCast(list.items.len);
}
