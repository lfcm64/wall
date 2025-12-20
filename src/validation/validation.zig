const std = @import("std");
const wasm = @import("../wasm/wasm.zig");

const Module = @import("../Module.zig");
const FunctionValidator = @import("Function.zig");

const Allocator = std.mem.Allocator;

const log = std.log.scoped(.validation);

pub fn validateModule(allocator: Allocator, module: *const Module) !void {
    try validateFuncSection(module);
    try validateCodeSection(allocator, module);
}

pub fn validateFuncSection(module: *const Module) !void {
    for (module.funcs.items) |func| {
        if (func >= module.functypes.items.len) return error.FuncIndexOutOfBounds;
    }
}

pub fn validateCodeSection(allocator: Allocator, module: *const Module) !void {
    for (0..module.code.items.len) |i| {
        var func_validator = try FunctionValidator.init(allocator, module, @intCast(i));
        defer func_validator.deinit();
        try func_validator.validate();
    }
}
