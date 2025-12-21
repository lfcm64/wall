const Context = @This();

const std = @import("std");
const wasm = @import("../wasm/wasm.zig");

const Payload = @import("event.zig").Payload;

const types = wasm.types;
const sections = wasm.sections;

const Allocator = std.mem.Allocator;
const Section = sections.Section;

allocator: std.mem.Allocator,

functypes: []types.FuncType = &[_]types.FuncType{},
imports: []types.Import = &[_]types.Import{},
funcs: []u32 = &[_]u32{},
tables: []types.Table = &[_]types.Table{},
memories: []types.Memory = &[_]types.Memory{},
globals: []types.Global = &[_]types.Global{},
exports: []types.Export = &[_]types.Export{},
start: ?u32 = null,

pub fn init(allocator: std.mem.Allocator) Context {
    return .{ .allocator = allocator };
}

pub fn deinit(self: *Context) void {
    self.allocator.free(self.functypes);
    self.allocator.free(self.imports);
    self.allocator.free(self.funcs);
    self.allocator.free(self.tables);
    self.allocator.free(self.memories);
    self.allocator.free(self.globals);
    self.allocator.free(self.exports);
}

pub fn receivePayload(self: *Context, payload: Payload) !void {
    switch (payload) {
        .type_section => |section| self.functypes = try section.collect(self.allocator),
        .import_section => |section| self.imports = try section.collect(self.allocator),
        .func_section => |section| self.funcs = try section.collect(self.allocator),
        .table_section => |section| self.tables = try section.collect(self.allocator),
        .memory_section => |section| self.memories = try section.collect(self.allocator),
        .global_section => |section| self.globals = try section.collect(self.allocator),
        .export_section => |section| self.exports = try section.collect(self.allocator),
        .start_section => |section| self.start = section.func_idx,
        else => {},
    }
}

pub fn typeOfFunc(self: *const Context, idx: u32) types.FuncType {
    const type_idx = self.funcs[idx];
    return self.functypes[type_idx];
}
