const Module = @This();

const std = @import("std");

const Parser = @import("parser/Parser.zig");
const Validator = @import("validator/Validator.zig");
const Compiler = @import("compiler/Compiler.zig");

const Instance = @import("runtime/Instance.zig");

const Allocator = std.mem.Allocator;

allocator: Allocator,
source: []const u8,

validator: Validator,
compiler: Compiler,

pub fn init(allocator: Allocator, source: []const u8) Module {
    return .{
        .allocator = allocator,
        .source = source,
        .validator = Validator.init(allocator),
        .compiler = Compiler.init(allocator),
    };
}

pub fn deinit(self: *Module) void {
    self.validator.deinit();
    self.compiler.deinit();
}

pub fn instantiate(self: *Module) !Instance {
    var parser = Parser.init(self.source);

    while (try parser.parseNext()) |event| {
        try self.validator.onEvent(event);
        try self.compiler.onEvent(event);
    }
    return Instance.init(self.compiler.ctx.llvm_module);
}
