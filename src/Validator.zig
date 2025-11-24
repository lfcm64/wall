const Validator = @This();

const std = @import("std");
const types = @import("module/types.zig");
const module = @import("module/module.zig");

const Parser = @import("Parser.zig");
const Store = @import("Store.zig");
const FunctionValidator = @import("Function.zig");

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
    const store = &self.store;
    switch (payload) {
        .module_header => |header| try self.validateModuleHeader(header),
        .type_section => |section| try store.appendResources(
            .func_types,
            self.allocator,
            section,
        ),
        .import_section => |section| try store.appendImports(
            self.allocator,
            section,
        ),
        .function_section => |section| try store.appendResources(
            .funcs,
            self.allocator,
            section,
        ),
        .table_section => |section| try store.appendResources(
            .tables,
            self.allocator,
            section,
        ),
        .memory_section => |section| try store.appendResources(
            .memories,
            self.allocator,
            section,
        ),
        .global_section => |section| try store.appendResources(
            .globals,
            self.allocator,
            section,
        ),
        .export_section => {},
        .start_section => {},
        .element_section => {},
        .code_section => |section| {
            var it = section.iter();
            var i: u32 = 0;
            while (try it.next()) |func| {
                var valid = try FunctionValidator.init(self.allocator, &self.store, func, i);
                defer valid.deinit();
                try valid.validate();
                i += 1;
            }
        },
        else => {},
    }
}

pub fn validateModuleHeader(self: *Validator, header: types.module.ModuleHeader) !void {
    if (self.state != .not_started) return error.UnexpectedHeader;
    self.state = .header;

    if (header.magic != 1836278016) return error.BadMagicNumber;
    if (header.version != 1) return error.BadVersionNumber;
}
